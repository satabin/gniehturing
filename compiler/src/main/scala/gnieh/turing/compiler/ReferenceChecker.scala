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

import scala.util.DynamicVariable

import symbol._
import util._
import tree._
import tree.worker.Traverser

/**
 * This checks that referenced names are defined and of the correct type
 *
 * @author Lucas Satabin
 *
 */
class ReferenceChecker(reporter: Reporter) extends Traverser {

  override def traverse(node: Node) = node match {
    case _ =>
      // just delegate to super method
      super.traverse(node)
  }

}