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

import scala.util.parsing.input.{ Position, NoPosition }
import scala.collection.mutable.Map

/**
 * @author Lucas Satabin
 *
 */
abstract class Symbol(val pos: Position,
                      val name: String,
                      val tpe: Type,
                      val scope: Scope) {

  /** Returns the list of params for this symbol if any*/
  def params: List[Symbol]

}

/** A symbol that may be called as next parameter, either a machine or a state */
trait CallableSymbol extends Symbol

/** A variable may be either a char or a tape */
case class VariableSymbol(override val pos: Position,
                          override val name: String,
                          override val tpe: Type)(scope: Scope)
    extends Symbol(pos, name, tpe, scope) {
  val params = Nil

  override def toString = name + ": " + tpe
}

case class StateSymbol(override val pos: Position,
                       override val name: String,
                       val params: List[Symbol])(scope: Scope)
    extends Symbol(pos, name, TState, scope)
    with CallableSymbol {

  override def toString =
    name + params.mkString("(", ", ", "): state")

}

object SyntheticState {

  var ind = 0

  def newState(scope: Scope) = {
    val res = StateSymbol(NoPosition, "synthetic$state$" + ind, Nil)(scope)
    ind += 1
    res
  }
}

case class MachineSymbol(override val pos: Position,
                         override val name: String,
                         val params: List[Symbol],
                         oracle: Boolean)(scope: Scope)
    extends Symbol(pos, name, TMachine(params.map(_.tpe)), scope)
    with CallableSymbol {

  override def toString =
    (if (oracle) "<oracle>" else "") + name +
      params.mkString("(", ", ", "): machine")

}

case class ModuleSymbol(override val pos: Position,
                        override val name: String)(scope: Scope)
    extends Symbol(pos, name, TModule, scope) {
  val params = Nil

  override def toString = name + ": module"

}

case class ToInferSymbol(override val pos: Position,
                         override val name: String)(scope: Scope)
    extends Symbol(pos, name, TUnknown, scope) {
  val params = Nil

  override def toString = name + ": ?"

}

case object NoSymbol extends Symbol(NoPosition, "no-name", TUnknown, NoScope) {
  val params = Nil

  override def toString = name + ": ?"

}

case class CharValueSymbol(override val pos: Position,
                           value: Char)(scope: Scope)
    extends Symbol(pos, "no-name", TChar, scope) {
  val params = Nil

  override def toString = value + ": char"
}