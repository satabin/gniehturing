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
    case CompilationUnit(module, uses, machines) =>
      // the scope of the compilation unit is the scope of the module it is
      // defined in, plus all the machines from the imports
      // if a machine is found twice, emit an error
      val moduleScope = module match {
        case Some(m) => m.symbol.scope
        case None => EmptyModuleSymbol.scope
      }
      val cuScope = new Scope(moduleScope)
      uses.foreach { use =>
        moduleScope.lookupModule(use.name) match {
          case Some(mod) =>
            // sets the module symbol to the use
            use.setSymbol(mod)
            // add the machines defined in its scope to the compilation 
            // unit scope
            mod.scope.foreach { sym =>
              try {
                cuScope.enter(sym)
              } catch {
                case _: CompilerException =>
                  // the machine already exists in this scope
                  reporter.error(currentFile, use.pos, "machine " +
                    sym.name + sym.params.mkString("(", ", ", ")") +
                    " in module " + mod +
                    " is already imported from another module")
              }
            }
          case None =>
            reporter.error(currentFile, use.pos, "Unknown module " + use.name)
        }
      }

      withScope(cuScope) {
        traverse(machines)
      }
    case machine: Machine =>
      withScope(machine.symbol.scope) {
        super.traverse(node)
      }
    case transition @ Transition(initial, _, _, _) =>
      transition.read.symbol match {
        case NoSymbol =>
          // the scope is simply the one from the state
          withScope(initial.symbol.scope) {
            super.traverse(node)
          }
        case sym =>
          // the symbol defines a new scope for this specific transition
          withScope(sym.scope) {
            super.traverse(node)
          }
      }
    case IdentRead(tape, name) =>
      //check that tape exists
      if (tape.isDefined)
        checkTape(tape.get)
      checkCharVar(name)
    case read: Read if read.tape.isDefined =>
      checkTape(read.tape.get)
    case Del(tape) =>
      if (tape.isDefined)
        checkTape(tape.get)
    case WriteChar(tape, char) =>
      if (tape.isDefined)
        checkTape(tape.get)
    case WriteString(tape, string) =>
      if (tape.isDefined)
        checkTape(tape.get)
    case WriteVar(tape, name) =>
      if (tape.isDefined)
        checkTape(tape.get)
      checkCharVar(name)
    case Left(tape, offset) =>
      if (tape.isDefined)
        checkTape(tape.get)
    case Right(tape, offset) =>
      if (tape.isDefined)
        checkTape(tape.get)
    case next @ NextIdent(name) =>
      checkStateVar(name)
      // set the symbol
      next.symbol = name.symbol
    case next @ NextCall(tape, name, args) =>
      if (tape.isDefined)
        checkTape(tape.get)
      // first traverse the arguments
      traverse(args)
      // then check the call
      checkCallable(name, args.map(_.symbol.tpe))
      // set the symbol
      next.symbol = name.symbol
    case _ =>
      // just delegate to super method
      super.traverse(node)
  }

  private def checkTape(ident: Ident) {
    currentScope.lookupVar(ident.name, Some(TTape)) match {
      case Some(sym) =>
        // ok, set the symbol
        ident.setSymbol(sym)
      case None =>
        // not found, error
        reporter.error(currentFile, ident.pos, "Unknown tape " + ident)
    }
  }

  private def checkCharVar(name: Ident) {
    currentScope.lookupVar(name.name, Some(TChar)) match {
      case Some(sym) =>
        // ok, set the symbol
        name.setSymbol(sym)
      case None =>
        // not found, error
        reporter.error(currentFile, name.pos, "Unknown character variable " + name)
    }
  }

  private def checkStateVar(name: Ident) {
    currentScope.lookupState(name.name, Nil) match {
      case Some(sym) =>
        // ok, set the symbol
        name.setSymbol(sym)
      case None =>
        // not found, error
        reporter.error(currentFile, name.pos, "Unknown state " + name)
    }
  }

  private def checkCallable(name: Ident, params: List[Type]) {
    currentScope.lookupCallable(name.name, params) match {
      case Some(sym) =>
        // ok, set the symbol
        name.setSymbol(sym)
      case None =>
        // not found, error
        reporter.error(currentFile, name.pos, "Unknown state or machine " +
          name + params.mkString("(", ", ", ")"))
    }
  }

}