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
package frontend

import tree._
import tree.worker._
import util.Reporter

/**
 * This phase simplifies and de-sugars the actions. For example
 * <pre>
 * write "test"
 * </pre>
 * becomes
 * <pre>
 * write 't' write 'e' write 's' write 't'
 * </pre>
 * <p />
 * Consecutive equal moves are also concatenated
 * <pre>
 * left left left
 * </pre>
 * becomes
 * <pre>
 * left 3
 * </pre>
 *
 * @author Lucas Satabin
 *
 */
abstract class ActionsSimplifier
    extends Step {

  val name = "simplify-actions"

  def apply(cu: CompilationUnit) =
    transformer.transform(cu).asInstanceOf[CompilationUnit]

  private object transformer extends ConservativeTransformer {

    override def transform(node: Node) = node match {
      case transition @ Transition(_, _, actions, next) =>

        val newActions = actions.flatMap {
          case write: WriteString =>
            transformWriteString(write)
          case action => List(action)
        }

        val actions2 = groupByActionType(newActions).flatMap { acts =>
          if (acts.forall(_.isInstanceOf[Move])) {
            val offset = acts.foldLeft(0) {
              case (akk, Right(_, n)) =>
                akk + n
              case (akk, Left(_, n)) =>
                akk - n
              case (akk, _) =>
                akk // should NEVER happen, but avoid compilation warning 
            }
            if (offset > 0)
              List(Right(acts.head.asInstanceOf[Move].tape, offset))
            else if (offset < 0)
              List(Left(acts.head.asInstanceOf[Move].tape, -offset))
            else
              Nil
          } else {
            acts
          }
        }

        transition.copy(actions = actions2)
          .setPos(transition.pos)
          .setSymbol(transition.symbol)
      case _ =>
        // just delegate to parent
        super.transform(node)
    }

    private def transformWriteString(write: WriteString) = {
      write.str.map { c =>
        WriteChar(write.tape, c)
          .setPos(write.pos)
          .setSymbol(write.symbol)
      }.toList
    }

    private def groupByActionType(acts: List[Action]): List[List[Action]] =
      acts match {
        case act :: rest =>
          val segment = acts.takeWhile(similarAction(act) _)
          segment :: groupByActionType(acts.drop(segment.length))
        case _ => Nil
      }

    private def similarAction(ref: Action)(other: Action) = (ref, other) match {
      case (Left(tape1, _), Left(tape2, _)) =>
        tape1 == tape2
      case (Left(tape1, _), Right(tape2, _)) =>
        tape1 == tape2
      case (Right(tape1, _), Right(tape2, _)) =>
        tape1 == tape2
      case (Right(tape1, _), Left(tape2, _)) =>
        tape1 == tape2
      case (_: Left | _: Right, _) =>
        false
      case (_, _: Left | _: Right) =>
        false
      case _ =>
        true
    }
  }
}