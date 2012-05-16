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
import scala.collection.mutable.Map

/**
 * @author Lucas Satabin
 *
 */
abstract class Symbol(val name: String, val tpe: Type) {

  private[this] var _owner: Option[Symbol] = None

  //  private[Symbol] val children = Map.empty[(String, List[Symbol]), Symbol]

  def owner = _owner
  def owner_=(sym: Option[Symbol]) = _owner = sym

  def setOwner(sym: Symbol): this.type = {
    // dereference from old owner children if any
    //    owner match {
    //      case Some(s) =>
    //        s.children -= this
    //      case None => // nothing to do
    //    }
    // set new owner
    _owner = Option(sym)
    // reference as child from new owner
    //    sym.children += this
    this
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

case class VariableSymbol(override val name: String,
                          override val tpe: Type)
    extends Symbol(name, tpe) {
  val params = Nil
}

case class StateSymbol(override val name: String,
                       val params: List[Symbol])
    extends Symbol(name, TState) {

}

object SyntheticState {

  var ind = 0

  def apply(owner: Symbol) = {
    val res = StateSymbol("synthetic$state$" + ind, Nil).setOwner(owner)
    ind += 1
    res
  }
}

case class MachineSymbol(override val name: String,
                         val params: List[Symbol],
                         orcale: Boolean)
    extends Symbol(name, TMachine(params.map(_.tpe)))

case class ModuleSymbol(override val name: String)
    extends Symbol(name, TModule) {
  val params = Nil
}

case class ToInferSymbol(override val name: String)
    extends Symbol(name, TUnknown) {
  val params = Nil
}

case object NoSymbol extends Symbol("no-name", TUnknown) {
  val params = Nil
}