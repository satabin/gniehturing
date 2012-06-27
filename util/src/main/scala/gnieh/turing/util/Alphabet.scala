/** *************************************************************************
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
 *  *************************************************************************
 */
package gnieh.turing.util

import scala.collection.immutable.{ NumericRange, SortedSet }

/** An alphabet contains ranges of characters.
 *
 *  @author Lucas Satabin
 *
 */
class Alphabet(
    private val characters: List[NumericRange[Char]] = Nil) {

  /** Adds a characters to the alphabet and returns the new updated alphabet.
   */
  def +(char: Char): Alphabet = {
    this + (char to char)
  }

  /** Adds a range of characters to the alphabet and returns the new
   *  updated alphabet.
   */
  def +(range: NumericRange[Char]): Alphabet = {
    val partitions = characters.groupBy { r =>
      if (r.last < range.first - 1)
        "before"
      else if (r.first > range.last + 1)
        "after"
      else
        "merge"
    }.withDefaultValue(Nil)

    val merged = partitions("merge").foldLeft(range)(merge _)

    new Alphabet(partitions("before") ::: List(merged) ::: partitions("after"))
  }

  def +(other: Alphabet): Alphabet =
    other.characters.foldLeft(this)(_ + _)

  /* merges two ranges. Only correct if they are continuous or intersecting */
  private def merge(range1: NumericRange[Char],
                    range2: NumericRange[Char]): NumericRange[Char] = {

    val min = math.min(range1.first, range2.first).asInstanceOf[Char]
    val max = math.max(range1.last, range2.last).asInstanceOf[Char]

    min to max

  }

  override def toString = {
    "{" + characters.map(range =>
      if (range.size == 1)
        range.first.toString
      else
        "'" + range.first + "' - '" + range.last + "'").mkString(", ") + "}"
  }

}

import scala.util.parsing.combinator.RegexParsers

object AlphabetParser extends RegexParsers {

  lazy val alphabet: Parser[Alphabet] =
    "{" ~> repsep(pattern, ",") <~ "}" ^^ {
      case ranges =>
        ranges.foldLeft(new Alphabet())(_ + _)
    }

  private lazy val pattern: Parser[NumericRange[Char]] =
    (
      ("'.'".r <~ "-") ~ "'.'".r ^^ {
        case c1 ~ c2 =>
          c1.charAt(1) to c2.charAt(1)
      }
      | "'.'".r ^^ (c => c.charAt(1) to c.charAt(1)))

}