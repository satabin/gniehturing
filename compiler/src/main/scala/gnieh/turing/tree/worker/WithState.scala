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
package tree.worker

import symbol._
import scala.util.DynamicVariable

/**
 * @author Lucas Satabin
 *
 */
trait WithState {

  protected[this] object _currentState extends DynamicVariable[Option[StateSymbol]](None)
  protected[this] def currentState = _currentState.value

  protected[this] def withState(state: Option[StateSymbol])(block: => Unit) {
    _currentState.withValue(state) {
      block
    }
  }

}