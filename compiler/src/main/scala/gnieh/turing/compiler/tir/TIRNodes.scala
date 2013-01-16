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
package gnieh.turing.compiler.tir

/** @author Lucas Satabin
 *
 */
sealed trait Reference
case object TapeRef extends Reference
final case class Parameter(index: Int) extends Reference

final case class TIRMachine(name: String,
                            parameters: List[TIRType],
                            locals: Map[String, TIRType],
                            body: List[TIRInstruction])

sealed trait TIRInstruction
final case class Label(name: String)

sealed trait Jump extends TIRInstruction {
  val label: String
}
final case class IfEqCharJump(ref: Char, id: String, label: String) extends Jump
final case class IfEqIdentJump(ref: String, id: String, label: String) extends Jump
final case class IfNoneJump(id: String, label: String) extends Jump
final case class JustJump(label: String) extends Jump

final case class Return(parameter: Parameter) extends TIRInstruction

case object Stop extends TIRInstruction

final case class Call(module: String, name: String, args: List[Any]) extends TIRInstruction

final case class WriteChar(c: Char, tape: String) extends TIRInstruction
final case class WriteIdent(ident: String, tape: String) extends TIRInstruction

final case class Del(tape: String) extends TIRInstruction

final case class MoveLeft(offset: Int) extends TIRInstruction
final case class MoveRight(offset: Int) extends TIRInstruction

final case class Alias(name: String, reference: Reference) extends TIRInstruction

final case class ReadTape(assigned: String, tape: String) extends TIRInstruction