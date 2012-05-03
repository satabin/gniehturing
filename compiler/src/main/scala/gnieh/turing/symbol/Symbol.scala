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
package symbol

import tree._
import scala.collection.mutable.Set

/**
 * @author Lucas Satabin
 *
 */
abstract class Symbol(val name: String, val tpe: Type, val owner: Option[Symbol]) {

  private[Symbol] val children = Set.empty[Symbol]

  owner match {
    case Some(sym) =>
      sym.children += this
    case None => // do nothing
  }

  /** Returns the list of params for this symbol if any*/
  def params: List[Symbol]

  def lookup(sym: String): List[Symbol] = {
    if (sym == name)
      List(this)
    else owner match {
      case Some(o) => o.lookup(sym)
      case None => Nil
    }
  }

}

class VariableSymbol(name: String, tpe: Type, owner: Symbol)
    extends Symbol(name, tpe, Some(owner)) {
  val params = Nil
}

class MachineSymbol(name: String,
                    val params: List[Symbol],
                    owner: Symbol)
    extends Symbol(name, TMachine(params.map(_.tpe)), Some(owner))