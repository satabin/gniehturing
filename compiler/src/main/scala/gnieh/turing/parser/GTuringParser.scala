/**
 * *************************************************************************
 *    This file is part of GTuring.                                        *
 *                                                                         *
 *  GTuring is free software: you can redistribute it and/or modify        *
 *  it under the terms of the GNU General Public License as published by   *
 *  the Free Software Foundation, either version 3 of the License, or      *
 *  (at your option) any later version.                                    *
 *                                                                         *
 *  GTuring is distributed in the hope that it will be useful,             *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 *  GNU General Public License for more details.                           *
 *                                                                         *
 *  You should have received a copy of the GNU General Public License      *
 *  along with GTuring.  If not, see <http://www.gnu.org/licenses/>.       *
 *                                                                         *
 * *************************************************************************
 */
package gnieh.turing
package parser

import util._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input._
import gnieh.turing.tree._

/**
 *
 * This parser allows the user to parse TMDL code and to construct an AST of this code.
 *
 * @author Lucas Satabin
 *
 */
object GTuringParser extends RegexParsers {

  /**
   * Parses a compilation unit
   * <unit> ::= <module>? <use>* <machine>*
   */
  lazy val unit: Parser[CompilationUnit] =
    opt(module) ~ rep(use) ~ rep(machine) ^^ {
      case mod ~ uses ~ machines => CompilationUnit(mod, uses, machines)
    }

  /**
   * <module> ::= `module' <ident>
   */
  lazy val module: Parser[Ident] =
    positioned("module" ~> ident)

  /**
   * <use> ::= `use' <ident>
   */
  lazy val use: Parser[Ident] =
    positioned("use" ~> ident)

  /**
   * <machine> ::= `oracle' <ident> `(' <param>* `)'
   *             | <ident> `(' <param>* `)' <tape>* <transition>+ `--'
   */
  lazy val machine: Parser[Machine] =
    positioned(
      "oracle" ~> decl ^^ {
        case (ident, params) => Machine(ident, params)
      }
        | decl ~ rep(tape) ~ before(currentStateName = None) {
          rep(transition)
        } <~ "--" ^^ {
          case (ident, params) ~ tapes ~ transitions =>
            Machine(ident, params, tapes, transitions, false)
        })

  lazy val decl: Parser[(Ident, List[Var])] =
    ident ~ paramList ^^ {
      case id ~ params => (id, params)
    }

  private lazy val paramList: Parser[List[Var]] =
    "(" ~> repsep(param, ",") <~ ")"

  /**
   * <param> ::= <ident> <type_annot>?
   * <type_annot> ::= `:' <tpe>
   */
  lazy val param: Parser[Var] =
    positioned(ident ~ opt(":" ~> tpe) ^^ {
      case id ~ tpe => Var(id, tpe)
    })

  /**
   * <tpe> ::= `char'
   *         | `state'
   *         | `tape'
   */
  lazy val tpe: Parser[Type] =
    "char" ^^^ TChar | "state" ^^^ TState | "tape" ^^^ TTape

  /**
   * <tape> ::= <ident> `:' `tape'
   */
  lazy val tape: Parser[Var] =
    positioned(ident <~ (":" ~ "tape") ^^ (Var(_, Some(TTape))))

  /**
   * <transition> ::= <initial_state>? `|' <read> `|' <action>* `|' <next>
   */
  lazy val transition: Parser[Transition] =
    (opt(initial) <~ "|") ~ (read <~ "|") ~ (rep(action) <~ "|") ~ next ^^ {
      case Some(init) ~ read ~ actions ~ next =>
        Transition(init, read, actions, next)
      case None ~ read ~ actions ~ next if currentStateName.isDefined =>
        Transition(
          InitialState(Ident(currentStateName.get), Nil), read, actions, next)
      case None ~ read ~ actions ~ next =>
        currentStateName = Some(newStateName)
        Transition(
          InitialState(Ident(currentStateName.get), Nil), read, actions, next)
    }

  /**
   * <initial_state> ::= <decl>
   *                   | <ident>
   */
  lazy val initial: Parser[InitialState] =
    positioned(decl ^^ InitialState.tupled | ident ^^ (InitialState(_, Nil)))

  /**
   * <read> ::= <affect>? <tape_prefix>? `any'
   *          | <affect>? <tape_prefix>? `all'
   *          | <tape_prefix>? `none'
   *          | <tape_prefix>? <char>
   *          | <tape_prefix>? <ident>
   */
  lazy val read: Parser[Read] =
    positioned(
      opt(affect) ~ opt(tapePrefix) <~ "any" ^^ {
        case affect ~ tape => AnyChar(affect, tape)
      }
        | opt(affect) ~ opt(tapePrefix) <~ "all" ^^ {
          case affect ~ tape => AllChar(affect, tape)
        }
        | opt(tapePrefix) <~ "none" ^^ NoneChar
        | opt(tapePrefix) ~ char ^^ {
          case tape ~ char => SingleChar(tape, char)
        }
        | opt(tapePrefix) ~ ident ^^ {
          case tape ~ name => IdentRead(tape, name)
        })

  /**
   * <action> ::= <tape_prefix>? `del'
   *            | <tape_prefix>? `write' <char>
   *            | <tape_prefix>? `write' <string>
   *            | <tape_prefix>? `write' <ident>
   *            | <tape_prefix>? `left' <number>?
   *            | <tape_prefix>? `right' <number>?
   */
  lazy val action: Parser[Action] =
    positioned(
      opt(tapePrefix) <~ "del" ^^ Del
        | opt(tapePrefix) ~ ("write" ~> char) ^^ {
          case tape ~ char => WriteChar(tape, char)
        }
        | opt(tapePrefix) ~ ("write" ~> string) ^^ {
          case tape ~ string => WriteString(tape, string)
        }
        | opt(tapePrefix) ~ ("write" ~> ident) ^^ {
          case tape ~ ident => WriteVar(tape, ident)
        }
        | opt(tapePrefix) ~ ("left" ~> opt(number)) ^^ {
          case tape ~ number => Left(tape, number.getOrElse(1))
        }
        | opt(tapePrefix) ~ ("right" ~> opt(number)) ^^ {
          case tape ~ number => Right(tape, number.getOrElse(1))
        })

  /**
   * <next> ::= `end'
   *          | ident
   *          | <tape_prefix>? ident `(' arg* `)'
   * <arg> ::= <next>
   *         | <char>
   *
   */
  lazy val next: Parser[Next] =
    positioned(
      "end" ^^^ End
        | opt(tapePrefix) ~ ident ~
        ("(" ~> repsep(next | char ^^ CharArg, ",") <~ ")") ^^ {
          case tape ~ name ~ args => NextCall(tape, name, args)
        }
        | ident ^^ NextIdent)

  /**
   * <tape_prefix> ::= <ident> `.'
   */
  lazy val tapePrefix: Parser[Ident] =
    ident <~ "."

  /**
   * <affect> ::= ident `<-'
   */
  lazy val affect: Parser[Var] =
    positioned(ident <~ "<-" ^^ (Var(_, Some(TChar))))

  lazy val char: Parser[Char] =
    "'.'".r ^^ (_.charAt(1))

  lazy val string: Parser[String] =
    "\"([^\"]|\\\")*\"".r ^^ (s => s.substring(1, s.length))

  lazy val number: Parser[Int] =
    "\\d+".r ^^ (_.toInt)

  lazy val ident: Parser[Ident] =
    positioned("[a-zA-Z_][a-zA-Z_0-9]*".r ^^ Ident)

  // helper methods

  private def before[T](action: => Unit)(p: => Parser[T]): Parser[T] = {
    new Parser[T] {
      def apply(in: Input) = {
        action
        p.apply(in)
      }
    }
  }

  private var currentStateName: Option[String] = None

  private var ind = 0

  private def newStateName = {
    val res = "synthetic$state$" + ind
    ind += 1
    res
  }

}