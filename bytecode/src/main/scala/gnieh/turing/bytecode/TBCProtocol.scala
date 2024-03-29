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
package gnieh.turing.bytecode

import sbinary._
import scala.annotation.tailrec

/**
 * @author Lucas Satabin
 *
 */
object TBCProtocol {

  private def readUnsigned(in: Input) = in.readByte.toInt & 0xFF

  object U1Format extends Format[Short] {
    def reads(in: Input) = readUnsigned(in).toShort

    def writes(out: Output, t: Short) =
      out.writeByte((t & 0xFF).toByte)
  }

  object U2Format extends Format[Int] {
    def reads(in: Input) = {
      val ch1 = readUnsigned(in)
      val ch2 = readUnsigned(in)
      ((ch1 << 8) + (ch2 << 0)).toInt & 0xFFFF
    }

    def writes(out: Output, t: Int) {
      out.writeByte(((t >>> 8) & 0xFF).toByte)
      out.writeByte(((t >>> 0) & 0xFF).toByte)
    }
  }

  object U4Format extends Format[Int] {
    def reads(in: Input) = {
      val ch1 = readUnsigned(in)
      val ch2 = readUnsigned(in)
      val ch3 = readUnsigned(in)
      val ch4 = readUnsigned(in)
      ((ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0))
    }

    def writes(out: Output, t: Int) {
      out.writeByte(((t >>> 24) & 0xFF).toByte)
      out.writeByte(((t >>> 16) & 0xFF).toByte)
      out.writeByte(((t >>> 8) & 0xFF).toByte)
      out.writeByte(((t >>> 0) & 0xFF).toByte)
    }
  }

  object CharFormat extends Format[Char] {
    def reads(in: Input) = ((readUnsigned(in) << 8) + readUnsigned(in)).toChar;
    def writes(out: Output, t: Char) = {
      out.writeByte(((t >>> 8) & 0xFF).toByte);
      out.writeByte(((t >>> 0) & 0xFF).toByte);
    }
  }

  import Operations._

  object TBCStringFormat extends Format[String] {
    def reads(in: Input) = {
      @scala.annotation.tailrec
      def intern(akk: String): String = read(in)(CharFormat) match {
        case ';' =>
          // end
          akk
        case c =>
          intern(akk + c)
      }
      intern("")
    }

    def writes(out: Output, s: String) {
      out.writeAll((s + ";").getBytes("ASCII"))
    }
  }

}