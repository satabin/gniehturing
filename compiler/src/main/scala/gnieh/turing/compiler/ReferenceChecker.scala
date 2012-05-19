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
import tree.worker._

/**
 * This checks that referenced names are defined and of the correct type
 *
 * @author Lucas Satabin
 *
 */
class ReferenceChecker(implicit val reporter: Reporter)
    extends Traverser
    with WithScope {

  override def traverse(node: Node) = node match {
    case machine: Machine =>
      withScope(machine.symbol.scope) {
        super.traverse(node)
      }
    case transition @ Transition(initial, _, _, _) =>
      transition.read.symbol match {
        case NoSymbol =>
        // the scope is simply the one from the state
        case sym =>
      }
      withScope(transition.symbol.scope) {
        super.traverse(node)
      }
    case InitialState(name, params) =>
      traverse(name)
      traverse(params)
    case AnyChar(affect, tape) =>
      if (affect.isDefined)
        traverse(affect.get)
      if (tape.isDefined)
        traverse(tape.get)
    case AllChar(affect, tape) =>
      if (affect.isDefined)
        traverse(affect.get)
      if (tape.isDefined)
        traverse(tape.get)
    case NoneChar(tape) =>
      if (tape.isDefined)
        traverse(tape.get)
    case SingleChar(tape, char) =>
      if (tape.isDefined)
        traverse(tape.get)
    case IdentRead(tape, name) =>
      if (tape.isDefined)
        traverse(tape.get)
      traverse(name)
    case Del(tape) =>
      if (tape.isDefined)
        traverse(tape.get)
    case WriteChar(tape, char) =>
      if (tape.isDefined)
        traverse(tape.get)
    case WriteString(tape, string) =>
      if (tape.isDefined)
        traverse(tape.get)
    case WriteVar(tape, name) =>
      if (tape.isDefined)
        traverse(tape.get)
      traverse(name)
    case Left(tape, offset) =>
      if (tape.isDefined)
        traverse(tape.get)
    case Right(tape, offset) =>
      if (tape.isDefined)
        traverse(tape.get)
    case NextIdent(name) =>
      traverse(name)
    case NextCall(tape, name, args) =>
      if (tape.isDefined)
        traverse(tape.get)
      traverse(name)
      traverse(args)
    case _ =>
      // just delegate to super method
      super.traverse(node)
  }

}