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

import util._
import tree._
import tree.worker._

/**
 * @author Lucas Satabin
 *
 */
object Runner {

  def apply(name: String, traverser: Traverser, verbose: Boolean)(implicit reporter: Reporter) =
    new TraverserRunner(name, reporter, traverser, verbose)

  def apply(name: String, transformer: ConservativeTransformer, verbose: Boolean)(implicit reporter: Reporter) =
    new ConservativeTransformerRunner(name, reporter, transformer, verbose)

}

sealed abstract class Runner(val name: String,
                             val reporter: Reporter,
                             val verbose: Boolean) {

  self =>

  type CompilationUnits = List[CompilationUnit]

  def andThen(next: Runner) =
    new Runner(self.name, reporter, self.verbose) {
      def run(unit: CompilationUnit) = self.run(unit)
      override def run(units: CompilationUnits) = {

        val res = super.run(units)

        if (!self.reporter.hasErrors_?)
          // if no error was found, apply next runner
          next.run(res)
        else
          // else just stop here and return the result
          res
      }
    }

  def run(unit: CompilationUnit): CompilationUnit

  def run(units: CompilationUnits): CompilationUnits =
    units.map(run _)

}

final class TraverserRunner(name: String,
                            reporter: Reporter,
                            traverser: Traverser,
                            verbose: Boolean)
    extends Runner(name, reporter, verbose) {

  def run(unit: CompilationUnit) = {
    if (verbose)
      reporter.info("Running phase " + name
        + " on " + unit.file.getCanonicalPath)
    traverser.traverse(unit)
    if (verbose)
      reporter.info("Phase " + name + " ran with " +
        (if (reporter.hasErrors_?) "error(s)" else "no errors"))

    unit
  }

}

final class ConservativeTransformerRunner(name: String,
                                          reporter: Reporter,
                                          transformer: ConservativeTransformer,
                                          verbose: Boolean)
    extends Runner(name, reporter, verbose) {

  def run(unit: CompilationUnit) = {
    if (verbose)
      reporter.info("Running phase " + name
        + " on " + unit.file.getCanonicalPath)
    val res = transformer.transform(unit).asInstanceOf[CompilationUnit]
    if (verbose)
      reporter.info("Phase " + name + " ran with " +
        (if (reporter.hasErrors_?) "error(s)" else "no errors"))

    res
  }

}