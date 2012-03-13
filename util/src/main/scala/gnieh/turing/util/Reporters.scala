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

import java.io._

/**
 * @author Lucas Satabin
 *
 */
trait Reporter {
  def report(msg: String, level: Level)
  /** Closes the reporter when the user is done with it */
  def close() { /* override if some close action is needed */ }
}

class ConsoleReporter extends Reporter {

  def report(msg: String, level: Level) {
    println("[" + level + "] " + msg)
  }

}

class FileReporter(fileName: String, append: Boolean = false) extends Reporter {

  val writer = new FileWriter(new File(fileName), append)

  def report(msg: String, level: Level) {
    writer.write("[" + level + "] " + msg)
    writer.flush
  }

  override def close() {
    writer.close
  }

}

sealed trait Level
case object Debug extends Level {
  override def toString = "DEBUG"
}
case object Info extends Level {
  override def toString = "INFO"
}
case object Warning extends Level {
  override def toString = "WARN"
}
case object Error extends Level {
  override def toString = "ERROR"
}
case object Fatal extends Level {
  override def toString = "FATAL"
}