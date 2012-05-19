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
package worker

/*
 * This file contains different worker classes to traverse and transform trees
 * @author Lucas Satabin
 *
 */

/**
 * A tree traverser that inspects all the nodes.
 */
class Traverser {
  def traverse(node: Node): Unit = node match {
    case CompilationUnit(module, uses, machines) =>
      if (module.isDefined)
        traverse(module.get)
      traverse(uses)
      traverse(machines)
    case Machine(name, params, tapes, transitions, oracle) =>
      traverse(name)
      traverse(params)
      traverse(tapes)
      traverse(transitions)
    case Transition(initial, read, actions, next) =>
      traverse(initial)
      traverse(read)
      traverse(actions)
      traverse(next)
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
    case _ => // do nothing
  }

  def traverse(nodes: List[Node]): Unit =
    nodes.foreach(traverse _)

}

/**
 * A tree transformer that conserves the tree structure.
 * The transformed node has the same type as the input node
 */
class ConservativeTransformer {
  def transform(node: Node): Node = node match {
    case CompilationUnit(module, uses, machines) =>
      CompilationUnit(
        transform(module).asInstanceOf[Option[Ident]],
        transform(uses).asInstanceOf[List[Ident]],
        transform(machines).asInstanceOf[List[Machine]])
    case Machine(name, params, tapes, transitions, oracle) =>
      Machine(
        transform(name).asInstanceOf[Ident],
        transform(params).asInstanceOf[List[Var]],
        transform(tapes).asInstanceOf[List[Var]],
        transform(transitions).asInstanceOf[List[Transition]],
        oracle)
    case Transition(initial, read, actions, next) =>
      Transition(
        transform(initial).asInstanceOf[InitialState],
        transform(read).asInstanceOf[Read],
        transform(actions).asInstanceOf[List[Action]],
        transform(next).asInstanceOf[Next])
    case InitialState(name, params) =>
      InitialState(
        transform(name).asInstanceOf[Ident],
        transform(params).asInstanceOf[List[Var]])
    case AnyChar(affect, tape) =>
      AnyChar(
        transform(affect).asInstanceOf[Option[Var]],
        transform(tape).asInstanceOf[Option[Ident]])
    case AllChar(affect, tape) =>
      AllChar(transform(affect).asInstanceOf[Option[Var]],
        transform(tape).asInstanceOf[Option[Ident]])
    case NoneChar(tape) =>
      NoneChar(transform(tape).asInstanceOf[Option[Ident]])
    case SingleChar(tape, char) =>
      SingleChar(transform(tape).asInstanceOf[Option[Ident]], char)
    case IdentRead(tape, name) =>
      IdentRead(
        transform(tape).asInstanceOf[Option[Ident]],
        transform(name).asInstanceOf[Ident])
    case Del(tape) =>
      Del(transform(tape).asInstanceOf[Option[Ident]])
    case WriteChar(tape, char) =>
      WriteChar(transform(tape).asInstanceOf[Option[Ident]], char)
    case WriteString(tape, string) =>
      WriteString(transform(tape).asInstanceOf[Option[Ident]], string)
    case WriteVar(tape, name) =>
      WriteVar(
        transform(tape).asInstanceOf[Option[Ident]],
        transform(name).asInstanceOf[Ident])
    case Left(tape, offset) =>
      Left(transform(tape).asInstanceOf[Option[Ident]], offset)
    case Right(tape, offset) =>
      Right(transform(tape).asInstanceOf[Option[Ident]], offset)
    case NextIdent(name) =>
      NextIdent(transform(name).asInstanceOf[Ident])
    case NextCall(tape, name, args) =>
      NextCall(
        transform(tape).asInstanceOf[Option[Ident]],
        transform(name).asInstanceOf[Ident],
        transform(args).asInstanceOf[List[Arg]])
    case _ => node
  }

  def transform(node: Option[Node]): Option[Node] =
    node.map(transform(_))

  def transform(nodes: List[Node]): List[Node] =
    nodes.map(transform(_))

}