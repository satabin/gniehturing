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

import symbol._
import tree.{ Type, TChar, TState }
import util.Reporter
import bytecode.{ BytecodeIO, Type => BCType, State => BCState, Character => BCChar }

import java.io.{ File, FilenameFilter, FileFilter }

/**
 * A resolver allows the user to resolve all modules and machines that are given
 * as libraries when compiling and add them to the symbol table.
 *
 * @author Lucas Satabin
 *
 */
class LibLoader(val options: Options)(implicit val reporter: Reporter) {

  private lazy val bytecodeIO = options.bcVersion match {
    case "2.1" =>
      BytecodeIO.forInstrType[bytecode.v2.Instruction]
    case version =>
      reporter.warning("Unknwon bytecode version " + version +
        ". Using 2.1 instead")
      BytecodeIO.forInstrType[bytecode.v2.Instruction]
  }

  def load {
    bytecodeIO match {
      case Some(io) =>

        // first load the .tbc files in the given path
        options.path.foreach { file =>
          if (file.isDirectory) {
            // if it is a directory, recursively search for ".tbc" files
            findTBCFiles(file).foreach { f =>
              io.loadTBCInterface(f).modules.foreach {
                case (name, machines) =>
                  // add each module
                  loadModule(name, machines)
              }
            }
          } else if (file.getName.endsWith(".tbc")) {
            // this is a .tbc file, load it
            io.loadTBCInterface(file).modules.foreach {
              case (name, machines) =>
                // add each module
                loadModule(name, machines)
            }
          } else {
            // it is neither a directory, nor a .tbc file, ignore it
            reporter.warning(file.getName +
              " is neither a directory, nor a .tbc file and is ignored in machine path")
          }
        }

      case None =>
        // ooootch, what happened there??? this should *NEVER* happen
        reporter.warning("No bytecode reader was found for version " +
          options.bcVersion + ". No library will be linked")
    }
  }

  // helper methods

  /* adds the module and machines defined in it to the top-level scope */
  private def loadModule(name: String, machines: List[(String, List[BCType])]) {

    val modScope = TopLevel.lookupModule(name) match {
      case Some(mod) =>
        // already exists, get the existing scope
        mod.scope
      case None =>
        // add new symbol and return scope
        val mod = ModuleSymbol(name)(new Scope(Some(TopLevel)))
        TopLevel.enter(mod)
        mod.scope
    }

    // XXX a machine with the same signature may be defined several times in the
    // same module (but in different files), that is why we first 
    // check that the machine is not already before adding it, to avoid
    // the exception when reentering a symbol
    machines.foreach {
      case (machineName, paramTypes) =>
        val macSym =
          MachineSymbol(name, paramTypes.map(fromBCType _), false)(modScope)
        if (modScope.contains(macSym))
          modScope.enter(macSym)
    }

  }

  private def fromBCType(bctype: BCType) = bctype match {
    case BCChar => VariableSymbol("_", TChar)(NoScope)
    case BCState => VariableSymbol("_", TState)(NoScope)
  }

  private def findTBCFiles(dir: File): List[File] = {
    // libraries in this directory
    val libs = dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String) =
        name.endsWith(".tbc")
    }).toList
    // sub directories to process
    val dirs = dir.listFiles(new FileFilter {
      def accept(f: File) = f.isDirectory
    }).toList

    libs ::: dirs.flatMap(findTBCFiles _)

  }

}