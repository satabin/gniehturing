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
import tree.worker._
import bytecode.v2.Instruction

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable

/**
 * @author Lucas Satabin
 *
 */
class BytecodeV2Translator {

  val machines = ListBuffer.empty[(String, List[Instruction])]

  object instructionsGenerator extends Traverser {

    private object currentFrame extends DynamicVariable[Frame](null)
    private var currentOffset = 0

    override def traverse(node: Node) = node match {
      case Machine(name, params, tapes, transitions, false) =>
        // a non-oracle machine => new frame with offset 0
        currentFrame.withValue(new Frame(name.name +
          params.map(_.symbol.tpe.toString.charAt(0)).mkString("(", "", ")"), 0)) {
          // push the parameters into the frame
          params.foreach(p => currentFrame.value.push(p.symbol))
          // push the declared tapes into the frame
          tapes.foreach(t => currentFrame.value.push(t.symbol))
          // size of the current frame as offset
          currentOffset += currentFrame.value.size
          traverse(transitions)
        }
      case Transition(InitialState(name, params), read, actions, next) =>

      case _ =>
        super.traverse(node)
    }
  }

}