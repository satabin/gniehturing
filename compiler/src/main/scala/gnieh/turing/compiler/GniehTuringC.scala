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

import java.io.File

import util._
import parser.TMDLFileParser

import scopt.immutable.OptionParser

/**
 * This is the main method, entry point for the gniehturing compiler
 *
 * @author Lucas Satabin
 *
 */
object GniehTuringC extends App {

  val optionsParser =
    new OptionParser[Options](Some("gtc"), Some("3.0"), true) {
      def options = Seq(
        help("h", "help", "display this help"),
        flag("v", "verbose", "enable the verbose mode") {
          c => c.copy(verbose = true)
        },
        opt("o", "output", "sets the output file name (default out.tbc)") {
          (s, o) => o.copy(outFile = new File(s))
        },
        opt("mp", "machinepath",
          "specify the machine path, all paths must be separated by a "
            + File.pathSeparator) {
            (s, o) =>
              o.copy(path = s.split(File.pathSeparator).map(f => new File(f)).toList)
          },
        opt("bc", "bytecode", "the version of generated bytecode (default 2.1)") {
          (s, o) => o.copy(bcVersion = s)
        },
        flag("no-output", "do not generate the compiled file, just print it to the console") {
          o => o.copy(generateOutput = false)
        },
        arglist("<file>...", ".tmdl files to compile") {
          (f, o) => o.copy(files = new File(f) :: o.files)
        })

      override def showUsage = println(usage)
    }

  optionsParser.parse(args, Options()) match {
    case Some(options) =>
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

      // the different passes as runners
      val runners =
        Runner("symbol-table-build", new SymbolTableBuilder, verbose) andThen
          // TODO type inference
          (Runner("reference-check", new ReferenceChecker, verbose) andThen
            Runner("actions-simplifier", new ActionsSimplifier, verbose))

      // let's go and do the job to transform the tree (frontend)!
      val result = runners.run(units)

      if (verbose)
        println(result)

      // ok now the tree looks like what we'd like to have, time to translate
      // to the intermediate representation (backend)
      if (!runners.reporter.hasErrors_?) {
        // TODO
      }
    case None =>
    // wrong option or argument, the help message was displayed
  }

}