/** *************************************************************************
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
 *  *************************************************************************
 */
package gnieh.turing
package compiler.tir

import util._

import scala.util.parsing.combinator.RegexParsers

/** @author Lucas Satabin
 *
 */
object TIRParser extends RegexParsers {

  override val whiteSpace = "(\\s|#.*)*".r

  lazy val tirMachine = (
    ident ~ ("(" ~> repsep(tirType, ",") <~ ")" ~ "{")
    ~ localDecls
    ~ rep(instruction | label) <~ "}")

  lazy val tirType: Parser[TIRType] = (
    "char" ^^^ TIRChar
    | "state" ^^^ TIRState
    | "tape" ^^^ TIRTape)

  lazy val localDecls =
    opt("tape" ~> repsep(ident, ",") <~ ";") ~
      opt("char" ~> repsep(ident, ",") <~ ";")

  lazy val label =
    ident <~ ":"

  lazy val instruction = (
    ("ifeq" ~> char ~ ident) ~ ("jump" ~> ident) ^^ {
      case char ~ variable ~ label => IfEqCharJump(char, variable, label)
    } |
    ("ifeq" ~> ident ~ ident) ~ ("jump" ~> ident) ^^ {
      case ref ~ variable ~ label => IfEqIdentJump(ref, variable, label)
    } |
    ("ifnone" ~> ident) ~ ("jump" ~> ident) ^^ {
      case variable ~ label => IfNoneJump(variable, label)
    } |
    "jump" ~> ident ^^ JustJump |
    "return" ~> parameter ^^ Return |
    "stop" ^^^ Stop |
    "call" ~> (ident <~ ".") ~ ident ~ ("(" ~> repsep(arg, ",") <~ ")") ^^ {
      case module ~ name ~ args => Call(module, name, args)
    } |
    ("write" ~> char) ~ ("to" ~> ident) |
    ("write" ~> ident) ~ ("to" ~> ident) |
    ("del_on" ~> ident) |
    ("move_left" ~> ident) ~ ("by" ~> number) |
    ("move_right" ~> ident) ~ ("by" ~> number) |
    (ident <~ ":=") ~ reference |
    (ident <~ ":=") ~ ("read_from_tape" ~> ident)) <~ ";"

  lazy val parameter =
    "@parameter\\d+".r ^^ (p => Parameter(p.substring(10).toInt))

  lazy val reference: Parser[Reference] = (
    "@tape" ^^^ TapeRef
    | parameter)

  lazy val char =
    "'.'".r ^^ (_.charAt(1))

  lazy val arg =
    reference | ident | char

  lazy val ident: Parser[String] =
    "[a-zA-Z_][-a-zA-Z_0-9]*".r

  lazy val number =
    "[0-9]+".r ^^ (_.toInt)

}