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
package gnieh.turing.compiler

import scala.util.parsing.input.{ Position, NoPosition }

/**
 * @author Lucas Satabin
 *
 */
class CompilerException(msg: String, inner: Throwable, val pos: Position)
    extends Exception(msg, inner) {

  def this(msg: String) = this(msg, null, NoPosition)
  def this(msg: String, pos: Position) = this(msg, null, pos)
  def this(msg: String, inner: Throwable) = this(msg, inner, NoPosition)

}