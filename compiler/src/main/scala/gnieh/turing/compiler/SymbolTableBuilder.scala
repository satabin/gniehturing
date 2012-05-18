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
 * This traverser allows the user to set the symbol for each declaration.
 *
 * @author Lucas Satabin
 *
 */
class SymbolTableBuilder(implicit val reporter: Reporter)
    extends Traverser
    with WithScope
    with WithState {

  override def traverse(node: Node) = try {
    node match {
      case CompilationUnit(Some(module), _, _) =>
        // if the module is already defined, get it, otherwise, create it
        val sym = TopLevel.lookupModule(module.name) match {
          case Some(mod) => mod
          case None =>
            val mod = ModuleSymbol(module.name)(new Scope(currentScope))
            // enter this new symbol into the parent scope
            currentScope.enter(mod)
            mod
        }
        module.setSymbol(sym)
        withScope(sym.scope) {
          super.traverse(node)
        }
      case CompilationUnit(None, _, _) =>
        // no module defined, set the default module scope
        withScope(EmptyModuleSymbol.scope) {
          super.traverse(node)
        }
      case Machine(name, params, tapes, transitions, oracle) =>
        // the new scope for this machine
        val machineScope = new Scope(currentScope)
        // first build parameter symbol table, which are in the new scope
        withScope(machineScope) {
          traverse(params)
        }
        // then create machine symbol
        val sym =
          MachineSymbol(name.name, params.map(_.symbol), oracle)(machineScope)
        // enter the new symbol in the current scope
        currentScope.enter(sym)

        node.setSymbol(sym)

        withState(None) {
          withScope(machineScope) {
            // then declared tapes
            traverse(tapes)
            // then transitions
            traverse(transitions)
          }
        }
      case Var(name, Some(tpe)) =>
        // variable with declared type
        val sym = VariableSymbol(name.name, tpe)(NoScope)
        currentScope.enter(sym)
        node.setSymbol(sym)
      case Var(name, None) =>
        // variable with no declared type
        val sym = ToInferSymbol(name.name)(NoScope)
        currentScope.enter(sym)
        node.setSymbol(sym)
      case Transition(Some(Named(name)), read, _, _) =>
        val state = currentState match {
          case None =>
            // new state defined
            StateSymbol(name.name, Nil)(new Scope(currentScope))
          case Some(state) if name.name != state.name =>
            // new state defined
            StateSymbol(name.name, Nil)(new Scope(currentScope))
          case Some(state) if name.name == state.name && state.params.nonEmpty =>
            // different parameter list, new state
            StateSymbol(name.name, Nil)(new Scope(currentScope))
          case _ =>
            currentState.get
        }
        currentScope.enter(state)

        // only the read part may declare some symbols in a transition
        withState(currentState) {
          withScope(state.scope) {
            traverse(read)
          }
        }
      case Transition(Some(Decl(name, params)), read, _, _) =>
        // the scope of the new state
        val newScope = new Scope(currentScope)

        // traverse parameters first
        withScope(newScope) {
          traverse(params)
        }

        val paramSymbols = params.map(_.symbol)

        val state = currentState match {
          case None =>
            // new state defined
            StateSymbol(name.name, paramSymbols)(newScope)
          case Some(state) if name.name != state.name =>
            // new state defined
            StateSymbol(name.name, paramSymbols)(newScope)
          case Some(state) if name.name == state.name
            && paramSymbols != state.params =>
            // different parameter list, new state
            StateSymbol(name.name, paramSymbols)(newScope)
          case _ =>
            currentState.get
        }

        // only the read part may declare some symbols in a transition
        withState(Some(state)) {
          withScope(state.scope) {
            traverse(read)
          }
        }
      case Transition(None, read, _, _) =>
        // no initial state given...
        currentState match {
          case Some(state) =>
            withScope(state.scope) {
              traverse(read)
            }
          case None =>
            // set the synthetic first state for this machine
            val state = SyntheticState.newState(new Scope(currentScope))
            currentScope.enter(state)
            withState(Some(state)) {
              withScope(state.scope) {
                traverse(read)
              }
            }
        }
      case _ =>
        // just delegate to super method
        super.traverse(node)
    }
  } catch {
    case e: CompilerException =>
      reporter.error(e.getMessage)
  }

}