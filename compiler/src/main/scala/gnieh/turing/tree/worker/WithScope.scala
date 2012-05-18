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
trait WithScope {

  protected[this] object _currentScope extends DynamicVariable[Scope](TopLevel)
  protected[this] def currentScope = _currentScope.value

  protected[this] def withScope(scope: Scope)(block: => Unit) {
    _currentScope.withValue(scope) {
      block
    }
  }

}