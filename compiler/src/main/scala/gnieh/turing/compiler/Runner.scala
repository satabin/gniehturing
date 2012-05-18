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

  def apply(reporter: Reporter, traverser: Traverser) =
    new TraverserRunner(reporter, traverser)

  def apply(reporter: Reporter, transformer: ConservativeTransformer) =
    new ConservativeTransformerRunner(reporter, transformer)

}

sealed abstract class Runner(reporter: Reporter) {

  self =>

  type CompilationUnits = List[CompilationUnit]

  def andThen[T](next: Runner) =
    new Runner(reporter) {
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

final class TraverserRunner(reporter: Reporter,
                            traverser: Traverser)
    extends Runner(reporter) {

  def run(unit: CompilationUnit) = {
    traverser.traverse(unit)
    unit
  }

}

final class ConservativeTransformerRunner(reporter: Reporter,
                                          transformer: ConservativeTransformer)
    extends Runner(reporter) {

  def run(unit: CompilationUnit) =
    transformer.transform(unit).asInstanceOf[CompilationUnit]

}