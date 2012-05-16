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
package gnieh.turing.bytecode.v2

/**
 * @author Lucas Satabin
 *
 */
case class TBCFile(version: String, modules: List[Module])

case class TBCInterface(version: String, modules: Map[String, List[String]])

case class Module(name: String, machines: List[Machine])

case class Machine(name: String,
                   paramTypes: List[Type],
                   instructions: List[Instruction]) {

  def nameString = name + paramTypes.mkString("(", "", ")")

}