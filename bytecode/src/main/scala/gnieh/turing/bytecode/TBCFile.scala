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

/**
 * @author Lucas Satabin
 *
 */
case class TBCFile[Instr](version: String, modules: List[Module[Instr]])

case class TBCInterface(version: String,
                        modules: Map[String, List[(String, List[Type])]])

case class Module[Instr](name: String, machines: List[Machine[Instr]])

case class Machine[Instr](name: String,
                          paramTypes: List[Type],
                          instructions: List[Instr]) {

  def nameString = name + paramTypes.mkString("(", "", ")")

}