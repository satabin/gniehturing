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
package test

import tree._
import parser._
import tree.worker.Traverser

/**
 * @author Lucas Satabin
 *
 */
object TestParser extends App {

  import GTuringParser._

  val parsingResult = parseAll(machine,
    """m(a: char, toto: state)
      plop: tape
      q | 'a' | del right 5 | d
      | test <- any | write "plop" toto.left | end--""") match {
      case Success(m, _) => posPrinter.traverse(m)
      case failure => println(failure)
    }

  object posPrinter extends Traverser {
    override def traverse(node: Node) {
      println(node.pos + " -> " + node)
      super.traverse(node)
    }
  }

}