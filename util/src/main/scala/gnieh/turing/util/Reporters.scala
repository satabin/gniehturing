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

  def report(msg: String, exc: Exception, level: Level)

  def report(msg: String, level: Level): Unit =
    report(msg, null, level)

  def debug(msg: String) =
    report(msg, Debug)

  def info(msg: String) =
    report(msg, Info)

  def warning(msg: String) =
    report(msg, Warning)

  def warning(msg: String, exc: Exception) =
    report(msg, exc, Warning)

  def error(msg: String) =
    report(msg, Error)

  def error(msg: String, exc: Exception) =
    report(msg, exc, Error)

  def hasErrors_? : Boolean
  def hasWarnings_? : Boolean

  /** Closes the reporter when the user is done with it */
  def close() { /* override if some close action is needed */ }
}

trait CountingReporter extends Reporter {

  private[this] var errorNb = 0
  private[this] var warningNb = 0

  abstract override def report(msg: String, exc: Exception, level: Level) {
    level match {
      case Error => errorNb += 1
      case Warning => warningNb += 1
      case _ => // nothing
    }
    super.report(msg, exc, level)
  }

  def errors = errorNb

  def hasErrors_? = errorNb > 0
  def hasWarnings_? = warningNb > 0

}

abstract class AccumulatingReporter extends Reporter {

}

abstract class ConsoleReporter extends Reporter {

  def report(msg: String, exception: Exception, level: Level) {
    println("[" + level + "] " + msg)
    if (exception != null)
      println(exception.getStackTraceString)
  }

}

abstract class FileReporter(fileName: String, append: Boolean = false) extends Reporter {

  val writer = new FileWriter(new File(fileName), append)

  def report(msg: String, exception: Exception, level: Level) {
    writer.write("[" + level + "] " + msg + "\n")
    if (exception != null)
      writer.write(exception.getStackTraceString + "\n")
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