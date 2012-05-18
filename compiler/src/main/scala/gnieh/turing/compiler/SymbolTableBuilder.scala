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
 * This traverser allows the user to set the symbol for each declaration.
 *
 * @author Lucas Satabin
 *
 */
class SymbolTableBuilder(implicit val reporter: Reporter) extends Traverser {

  object currentOwner extends DynamicVariable[Symbol](TopLevel)
  def owner_! = currentOwner.value

  var currentState: Option[StateSymbol] = None

  def withOwner(owner: Symbol)(block: => Unit) {
    currentOwner.withValue(owner) {
      block
    }
  }

  override def traverse(node: Node) = node match {
    case CompilationUnit(Some(module), _, _) =>
      // 
      val sym = ModuleSymbol(module.name).setOwner(TopLevel)
      module.setSymbol(sym)
      withOwner(sym) {
        super.traverse(node)
      }
    case Machine(name, params, tapes, transitions, oracle) =>
      // first build parameter symbol table
      traverse(params)
      // then create machine symbol
      val sym =
        MachineSymbol(name.name, params.map(_.symbol), oracle)
      // set this symbol as parameters owner
      params.foreach(_.symbol.setOwner(sym))

      // sets the module as owner, if no module defined, 
      // this is the default empty module
      owner_! match {
        case TopLevel =>
          sym.setOwner(EmptyModuleSymbol)
        case owner =>
          sym.setOwner(owner)
      }
      node.setSymbol(sym)
      withOwner(sym) {
        // then declared tapes
        traverse(tapes)
        // then transitions
        traverse(transitions)
      }
      // cleanup current state
      currentState = None
    case Var(name, Some(tpe)) =>
      // variable with declared type
      val sym = VariableSymbol(name.name, tpe)
      node.setSymbol(sym)
      sym.setOwner(owner_!)
    case Transition(Some(Named(name)), read, _, _) =>
      val state = currentState match {
        case None =>
          // new state defined
          StateSymbol(name.name, Nil).setOwner(owner_!)
        case Some(state) if name.name != state.name =>
          // new state defined
          StateSymbol(name.name, Nil).setOwner(owner_!)
        case Some(state) if name.name == state.name && state.params.nonEmpty =>
          // different parameter list, new state
          StateSymbol(name.name, Nil).setOwner(owner_!)
        case _ =>
          currentState.get
      }
      currentState = Some(state)

      // only the read part may declare some symbols in a transition
      withOwner(state) {
        traverse(read)
      }
    case Transition(Some(Decl(name, params)), read, _, _) =>
      // traverse parameters first
      traverse(params)

      val paramSymbols = params.map(_.symbol)

      val state = currentState match {
        case None =>
          // new state defined
          StateSymbol(name.name, paramSymbols).setOwner(owner_!)
        case Some(state) if name.name != state.name =>
          // new state defined
          StateSymbol(name.name, paramSymbols).setOwner(owner_!)
        case Some(state) if name.name == state.name
          && paramSymbols != state.params =>
          // different parameter list, new state
          StateSymbol(name.name, paramSymbols).setOwner(owner_!)
        case _ =>
          currentState.get
      }
      currentState = Some(state)

      // set this symbol as parameters owner
      params.foreach(_.symbol.setOwner(state))

      // only the read part may declare some symbols in a transition
      withOwner(state) {
        traverse(read)
      }
    case Transition(None, read, _, _) =>
      // no initial state given...
      currentState match {
        case Some(state) =>
          withOwner(state) {
            traverse(read)
          }
        case None =>
          // set the synthetic first state for this machine
          val state = SyntheticState(owner_!)
          currentState = Some(state)
          withOwner(state) {
            traverse(read)
          }
      }
    case _ =>
      // just delegate to super method
      super.traverse(node)
  }

}