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
package compiler
package backend

import symbol._
import tree._

import scala.collection.mutable.LinkedHashMap

/**
 * A frame contains accessible symbols with their offset
 *
 * @author Lucas Satabin
 *
 */
class Frame(val name: String,
            val offset: Int,
            val parent: Option[Frame] = None) {

  private[this] var internalOffset = 0
  private[this] val symbols = LinkedHashMap.empty[Symbol, Int]

  /** Pushes the symbol in this frame and returns its offset inside this frame */
  def push(sym: Symbol) = {
    val off = internalOffset
    symbols(sym) = off
    sym.tpe match {
      case TChar | TTape =>
        internalOffset += 1
      case TState =>
        internalOffset += 3
      case _ =>
      // nothing else than characters, states and tapes 
      // may be pushed in a frame
    }
    off
  }

  def offsetOf(sym: Symbol) =
    symbols.getOrElse(sym, -1)

  /** Returns the size of this frame */
  def size =
    symbols.keys.map(_.tpe match {
      case TChar | TTape => 1
      case TState => 3
      case _ => 0 // should never happen
    }).sum

}