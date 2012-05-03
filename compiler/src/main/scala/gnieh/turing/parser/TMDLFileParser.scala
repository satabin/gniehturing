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
package parser

import util.Arm._
import util.{ Reporter, Error => GTError }
import java.io._

import tree.CompilationUnit

/**
 * This class allows the user to parse a bunch of TDML files.
 *
 * @author Lucas Satabin
 *
 */
class TMDLFileParser(val inputFiles: List[File], reporter: Reporter) {

  /** Parses the input file and returns the parsed compilation unit */
  private def parse(file: File) = {
    using(new FileReader(file)) { reader =>
      import GTuringParser._

      parseAll(unit, reader) match {
        case Success(res, _) => Some(res)
        case failure =>
          reporter.report(failure.toString, GTError)
          None
      }
    }
  }

  /**
   * Parses all input files and returns the result of the successfully
   * parsed ones
   */
  def parseAllFiles: List[CompilationUnit] =
    inputFiles.foldLeft(List[CompilationUnit]()) { (list, file) =>
      parse(file) match {
        case Some(unit) => unit :: list
        case None => list
      }
    }

}