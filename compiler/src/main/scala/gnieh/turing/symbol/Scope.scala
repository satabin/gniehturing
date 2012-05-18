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
package symbol

import tree.Type
import compiler.CompilerException

import scala.collection.mutable.Map

/**
 * @author Lucas Satabin
 *
 */
class Scope(parent: Option[Scope] = None) {

  // the symbols defined in this scope, can be search by name and argument types
  private[this] var symbols = Map.empty[String, Map[List[Type], Symbol]]

  /** Enters the given symbol in this scope */
  def enter(sym: Symbol) {
    val name = sym.name
    val params = sym.params.map(_.tpe)
    lookupInThis(name, params) match {
      case Some(found) if found.tpe == sym.tpe =>
        // it already exists in this scope
        throw new CompilerException(found + " already exists in current scope")
      case _ =>
        // enter it
        val byName = symbols.getOrElseUpdate(name, Map.empty[List[Type], Symbol])
        byName(params) = sym
    }
  }

  /** Deletes the given symbol from this scope if it is defined in it */
  def delete(sym: Symbol) {
    val name = sym.name
    val params = sym.params.map(_.tpe)
    lookupInThis(name, params) match {
      case Some(_) =>
        // found, delete it
        symbols(name).remove(params)
        if (symbols(name).isEmpty) {
          // that was the only symbol with this name, remove it
          symbols.remove(name)
        }
      case None =>
      // not found, just ignore
    }
  }

  def lookupVar(name: String, tpe: Option[Type] = None): Option[VariableSymbol] =
    (lookupInThis(name, Nil), tpe) match {
      case (Some(sym: VariableSymbol), Some(tpe)) if sym.tpe == tpe =>
        // found, with right type!!
        Some(sym)
      case (Some(sym: VariableSymbol), None) =>
        // found, and no required type!!
        Some(sym)
      case _ if parent.isDefined =>
        // not found delegate to parent if any
        parent.get.lookupVar(name, tpe)
      case _ =>
        // not found and no parent
        None
    }

  def lookupMachine(name: String, args: List[Type]): Option[MachineSymbol] =
    lookupInThis(name, args) match {
      case Some(sym: MachineSymbol) =>
        // found, youpi!!
        Some(sym)
      case _ if parent.isDefined =>
        // not found delegate to parent if any
        parent.get.lookupMachine(name, args)
      case _ =>
        // not found and no parent
        None
    }

  def lookupState(name: String, args: List[Type]): Option[StateSymbol] =
    lookupInThis(name, args) match {
      case Some(sym: StateSymbol) =>
        // found, youpi!!
        Some(sym)
      case _ if parent.isDefined =>
        // not found delegate to parent if any
        parent.get.lookupState(name, args)
      case _ =>
        // not found and no parent
        None
    }

  def lookupModule(name: String): Option[ModuleSymbol] =
    lookupInThis(name, Nil) match {
      case Some(sym: ModuleSymbol) =>
        Some(sym)
      case _ =>
        // not found (modules are not hierarchical, so no parent)
        None
    }

  def lookupCallable(name: String, args: List[Type]): Option[CallableSymbol] =
    lookupInThis(name, args) match {
      case Some(sym: CallableSymbol) =>
        Some(sym)
      case _ if parent.isDefined =>
        // not found delegate to parent if any
        parent.get.lookupCallable(name, args)
      case _ =>
        // not found and no parent
        None
    }

  override def toString =
    "Scope {\n" + symbols.values.flatMap(_.values).mkString("  ", "\n  ", "\n") + "}"

  // helper methods

  private def lookupInThis(name: String, args: List[Type]) =
    symbols.get(name).flatMap(_.get(args))

}

/** The top-level symbol contains all the modules */
case object TopLevel extends Scope()
case object NoScope extends Scope()