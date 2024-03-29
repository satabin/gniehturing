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
package gnieh.turing.tree

/**
 * @author Lucas Satabin
 *
 */
sealed trait Type
case object TChar extends Type {
  override def toString = "char"
}
case object TState extends Type {
  override def toString = "state"
}
case object TTape extends Type {
  override def toString = "tape"
}
case class TMachine(params: List[Type]) extends Type {
  override def toString = params.mkString("(", ", ", ")") + ": state"
}
case class TTransition(params: List[Type]) extends Type {
  override def toString = params.mkString("(", ", ", ")") + ": state"
}
case object TModule extends Type {
  override def toString = "module"
}
case object TUnknown extends Type {
  override def toString = "?"
}