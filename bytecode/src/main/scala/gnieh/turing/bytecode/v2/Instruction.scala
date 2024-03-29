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
package gnieh.turing.bytecode
package v2

/**
 * A TBC instruction.
 *
 * @author Lucas Satabin
 *
 */
sealed trait Instruction

case class Pop(n: Short) extends Instruction
case class Read(tape: Byte, compare: Short, offset: Int, register: Byte) extends Instruction
case class ARead(tape: Byte, offset: Int, register: Byte) extends Instruction
case class SRead(tape: Byte, offset: Int, register: Byte) extends Instruction
case class Write(tape: Byte, char: Short) extends Instruction
case class SWrite(tape: Byte, offset: Int, register: Byte) extends Instruction
case class Movep(tape: Byte, of: Short) extends Instruction
case class Movem(tape: Byte, of: Short) extends Instruction
case class Jump(offset: Int, register: Byte) extends Instruction
case class Loadl(value: Short) extends Instruction
case class Loadc(tape: Byte) extends Instruction
case class Load(offset: Int, register: Byte) extends Instruction
case class SLoad(offset: Int, register: Byte) extends Instruction
case class Call(tape: Byte, name: String, paramTypes: List[Type]) extends Instruction
case class Return(offset: Int, register: Byte) extends Instruction
case object End extends Instruction
case class TAlloc(size: Int) extends Instruction