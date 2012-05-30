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

import parser._
import util._
import frontend.Step
import tree.CompilationUnit

import java.io.File

/**
 * An instance of the compiler.
 *
 * @author Lucas Satabin
 *
 */
abstract class Compiler(val reporter: Reporter,
                        val options: Options) {

  type CompilationUnits = List[CompilationUnit]

  // file currently being processed
  var currentFile: File = _

  val frontendSteps: List[Step]

  /** Runs the compiler with the given options */
  def gogogo {
    import options._
    implicit val reporter = new ConsoleReporter with CountingReporter

    // parses the files
    val parser = new TMDLFileParser(options.files)
    val units = parser.parseAllFiles

    // load the linked libraries
    if (verbose)
      reporter.info("Loading linked libraries from "
        + path.mkString(File.pathSeparator))
    val libloader = new LibLoader(options)
    libloader.load

    // the different frontend steps are executed

    val afterFrontendSteps = executeSteps(frontendSteps, units)

    if (verbose)
      reporter.info(afterFrontendSteps.mkString("\n\n==================\n"))

    // ok now the tree looks like what we'd like to have, time to translate
    // to the intermediate representation (backend)

  }

  // helper methods

  @scala.annotation.tailrec
  private def executeSteps(steps: List[Step],
                           units: CompilationUnits): CompilationUnits = steps match {
    case step :: rest =>
      val afterStep = units.map { u =>
        currentFile = u.file
        if (options.verbose)
          reporter.info("applying " + step.name + " to file " + currentFile)
        step(u)
      }
      if (!reporter.hasErrors_?)
        executeSteps(rest, afterStep)
      else
        afterStep
    case Nil => units
  }

}