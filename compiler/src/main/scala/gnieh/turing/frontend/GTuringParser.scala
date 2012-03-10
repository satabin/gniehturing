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
package gnieh.turing.frontend

import org.parboiled.scala._

/**
 *
 * This parser allows the user to parse TMDL code and to construct an AST of this code.
 *
 * @author Lucas Satabin
 *
 */
object GTuringParser extends Parser {

  /**
   * Parses a compilation unit
   * <unit> ::= <module>? <use>* <machine>*
   */
  def unit = rule {
    whitespace ~ optional(module) ~
      zeroOrMore(use) ~ zeroOrMore(machine) ~~> CompilationUnit
  }

  /**
   * <module> ::= `module' <ident>
   */
  def module: Rule1[Ident] = rule {
    "module" ~ ident
  }

  /**
   * <use> ::= `use' <ident>
   */
  def use: Rule1[Ident] = rule {
    "use" ~ ident
  }

  /**
   * <machine> ::= `oracle' <ident> `(' <param>* `)'
   *             | <ident> `(' <param>* `)' <tape>* <transition>+ `--'
   */
  def machine /*: Rule1[Machine]*/ = rule {
    "oracle" ~ decl ~~>
      ((ident: Ident, params: List[Var]) => Machine(ident, params)) |
      decl ~ zeroOrMore(tape) ~ zeroOrMore(transition) ~~>
      ((ident: Ident, params: List[Var], tapes: List[Var],
        transitions: List[Transition]) =>
        Machine(ident, params, tapes, transitions, false)) ~ "--" ~ whitespace
  }

  def decl: Rule2[Ident, List[Var]] = rule {
    ident ~ paramList ~ whitespace
  }

  private def paramList: Rule1[List[Var]] = rule {
    "(" ~ zeroOrMore(param, "," ~ whitespace) ~~> (l => l) ~ ")"
  }

  /**
   * <param> ::= <ident> <type_annot>?
   * <type_annot> ::= `:' <tpe>
   */
  def param: Rule1[Var] = rule {
    ident ~ optional(":" ~ whitespace ~ tpe) ~~> Var
  }

  /**
   * <tpe> ::= `char'
   *         | `state'
   *         | `tape'
   */
  def tpe: Rule1[Type] = rule {
    "char" ~ push(TChar) |
      "state" ~ push(TState) |
      "tape" ~ push(TTape)
  }

  /**
   * <tape> ::= <ident> `:' `tape'
   */
  def tape: Rule1[Var] = rule {
    ident ~ ":" ~ whitespace ~ "tape" ~ whitespace ~~> ((ident: Ident) => Var(ident, Some(TTape)))
  }

  /**
   * <transition> ::= <initial_state>? `|' <read> `|' <action>* `|' <next>
   */
  def transition = rule {
    optional(initial) ~ "|" ~ whitespace ~ read ~ "|" ~ whitespace ~
      zeroOrMore(action) ~ "|" ~ whitespace ~ next ~~>
      Transition
  }

  /**
   * <initial_state> ::= <decl>
   *                   | <ident>
   */
  def initial: Rule1[InitialState] = rule {
    decl ~~> Decl |
      ident ~~> Named
  }

  /**
   * <read> ::= <affect>? <tape_prefix>? `any'
   *          | <affect>? <tape_prefix>? `all'
   *          | <tape_prefix>? `none'
   *          | <tape_prefix>? <char>
   *          | <tape_prefix>? <ident>
   */
  def read: Rule1[Read] = rule {
    optional(affect) ~ optional(tapePrefix) ~ "any" ~ whitespace ~~> AnyChar |
      optional(affect) ~ optional(tapePrefix) ~ "all" ~ whitespace ~~> AllChar |
      optional(tapePrefix) ~ "none" ~ whitespace ~~> NoneChar |
      optional(tapePrefix) ~ "'" ~ (normalChar | escapedChar) ~>
      (_.charAt(0)) ~~> SingleChar ~ "'" ~ whitespace
  }

  /**
   * <action> ::= <tape_prefix>? `del'
   *            | <tape_prefix>? `write' <char>
   *            | <tape_prefix>? `write' <string>
   *            | <tape_prefix>? `write' <ident>
   *            | <tape_prefix>? `left' <number>?
   *            | <tape_prefix>? `right' <number>?
   */
  def action: Rule1[Action] = rule {
    optional(tapePrefix) ~ "del" ~ whitespace ~~> Del |
      optional(tapePrefix) ~ "write" ~ whitespace ~ "'" ~ ANY ~> (_.charAt(0)) ~ "'" ~~>
      WriteChar ~ whitespace |
      optional(tapePrefix) ~ "write" ~ whitespace ~ "\"" ~ zeroOrMore(normalChar | escapedChar) ~> (s => s) ~
      "\"" ~~> WriteString ~ whitespace |
      optional(tapePrefix) ~ "write" ~ whitespace ~ ident ~~> WriteVar ~ whitespace |
      optional(tapePrefix) ~ "left" ~ whitespace ~ optional(oneOrMore(number) ~> (_.mkString("", "", "").toInt)) ~~>
      (_ getOrElse 1) ~~> Left ~ whitespace |
      optional(tapePrefix) ~ "right" ~ whitespace ~ optional(oneOrMore(number) ~> (_.mkString("", "", "").toInt)) ~~>
      (_ getOrElse 1) ~~> Right ~ whitespace
  }
  /**
   * <next> ::= `end'
   *          | ident
   *          | <tape_prefix>? ident `(' arg* `)'
   * <arg> ::= <next>
   *         | <char>
   *
   */
  def next: Rule1[Next] = rule {
    "end" ~ whitespace ~ push(End) |
      optional(tapePrefix) ~ ident ~ "(" ~ zeroOrMore(next |
        (ANY ~> (s => CharArg(s charAt 0))), ",") ~ ")" ~ whitespace ~~> NextCall |
      ident ~~> NextIdent
  }

  /**
   * <tape_prefix> ::= <ident> `.'
   */
  def tapePrefix: Rule1[Ident] = rule {
    ident ~ "."
  }

  /**
   * <affect> ::= ident `<-'
   */
  def affect = rule {
    ident ~~> (Var(_, Some(TChar))) ~ "<-" ~ whitespace
  }

  private def escapedChar: Rule0 = rule {
    "\\" ~ anyOf("\"\\bfnrt")
  }

  private def normalChar: Rule0 = rule {
    !anyOf("\"\\") ~ ANY
  }

  private def letter = rule {
    "a" - "z" | "A" - "Z"
  }

  private def number = rule {
    "0" - "9"
  }

  def ident = rule {
    (letter ~> (s => s) ~ zeroOrMore(letter | number | "_") ~> (s => s)) ~~>
      ((first, last) => first + last) ~~> Ident ~ whitespace
  }

  private def whitespace: Rule0 = rule {
    zeroOrMore(anyOf(" \n\r\t\f"))
  }

  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ whitespace
    else
      str(string)

}