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
package gnieh.turing.util

import java.io.FileNotFoundException

/**
 * Object defining methods for automatic resources management
 *
 * @author Lucas Satabin
 *
 */
object Arm {

  /** Something that can be closed */
  type Closeable = { def close() }

  /** Alias for PartialFunction */
  type ==>[Param, Ret] = PartialFunction[Param, Ret]

  /** default handler: rethrow the exception */
  implicit val defaultHandler: Throwable ==> Nothing = null

  /**
   * Using environment
   * Example:
   * <pre>
   * using(new FileOutputStream("test.txt")) {fos =>
   *   fos.write(...)
   * } {
   *   case e: FileNotFoundException => ...
   * }
   * </pre>
   */
  def using[T, U >: Null <: Closeable](cl: => U)(body: U => T)(implicit handler: PartialFunction[Throwable, T]): T = {
    // XXX `truie' is needed because the value of a call-by-name parameter is recomputed at each access
    // so each time we use `cl', we may receive a new instance of U
    // Using a `lazy parameter' (see https://lampsvn.epfl.ch/trac/scala/ticket/240) here is not possible
    // We cannot use `lazy val truie = cl' because we access `truie' in the finally block.
    // If the call to `cl' raises an exception, the same error will be raised in the finally block
    // because `truie' was not initialized, and the initialization will be done again, raising the same
    // exception.
    var truie: U = null
    try {
      truie = cl
      body(truie)
    } catch {
      case t if (handler != null && handler.isDefinedAt(t)) =>
        handler(t)
    } finally {
      if (truie != null)
        truie.close
    }
  }

}