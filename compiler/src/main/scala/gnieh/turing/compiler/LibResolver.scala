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

import symbol.TopLevel
import util.Reporter
import bytecode.BytecodeIO

/**
 * A resolver allows the user to resolve all modules and machines that are given
 * as libraries when compiling and add them to the symbol table.
 *
 * @author Lucas Satabin
 *
 */
class LibResolver(val options: Options)(implicit val reporter: Reporter) {

  private val bytecodeIO = options.bcVersion match {
    case "2.1" =>
      BytecodeIO.forInstrType[bytecode.v2.Instruction]
    case version =>
      reporter.warning("Unknwon bytecode version " + version +
        ". Using 2.1 instead")
      BytecodeIO.forInstrType[bytecode.v2.Instruction]
  }

  /* adds the module and machines defined in it to the top-level scope */
  private def loadModule(module: Map[String, List[String]]) {

  }

  bytecodeIO match {
    case Some(io) =>

      // first load the .tbc files in the given path
      options.path.foreach { file =>
        if (file.isDirectory) {
          // if it is a directory, recursively search for ".tbc" files
        } else if (file.getName.endsWith(".tbc")) {
          // this is a .tbc file, load it
          val itf = io.loadTBCInterface(file)
          itf.modules.foreach { module =>
            // add each module
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