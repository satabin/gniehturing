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

import java.io.File

/**
 * A BytecodeIO allows the user to have a generic interface with
 * a .tbc file.
 *
 * @author Lucas Satabin
 *
 */
trait BytecodeIO[Instr] {

  /** Loads the entire .tbc file */
  def loadTBCFile(file: File): TBCFile[Instr]

  /** Writes the .tbc file */
  def writeTBCFile(tbc: TBCFile[Instr], file: File)

  /** Loads the interface of modules and machines defined in this .tbc file*/
  def loadTBCInterface(file: File): TBCInterface

}

object BytecodeIO {

  /** Returns the BytecodeIO associated to the given instruction type. */
  def forInstrType[Instr: Manifest]: Option[BytecodeIO[Instr]] =
    if (manifest[Instr] <:< ClassManifest.fromClass(classOf[v2.Instruction]))
      Some(v2.BytecodeProtocol.asInstanceOf[BytecodeIO[Instr]])
    else
      None

}