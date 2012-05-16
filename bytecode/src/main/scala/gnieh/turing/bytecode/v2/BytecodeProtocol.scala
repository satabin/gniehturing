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
package v2

import sbinary._

/**
 * Allows the user to read/write a TBC v2.1 file.
 * The structure of such a file is the following:
 * <pre>
 * TBCFile {
 *   u4 magic;
 *   u1 major;
 *   u1 minor;
 *   u4 modules_count;
 *   module module_table[modules_count] {
 *     string module_name;
 *     u4 module_machines_count;
 *     (string, u4) machines[module_machines_count];
 *   };
 *   u4 machines_count;
 *   machine machine_table[machines_count] {
 *     u4 instructions_count;
 *     instruction instructions[instructions_count];
 *   };
 * };
 * </pre>
 *
 * @author Lucas Satabin
 *
 */
object BytecodeProtocol extends DefaultProtocol {

  import Operations._
  import TBCProtocol._

  implicit object InstructionFormat extends Format[Instruction] {

    def reads(in: Input) = {

      def tapeRegister = {
        val byte = read[Byte](in)
        ((byte & 0xf).toByte, ((byte & 0xf0) >> 4).toByte)
      }

      read[Byte](in) match {
        case 0x00 =>
          // drop unused byte
          read[Byte](in)
          val pop = Pop(read(in)(U1Format))
          // drop unused bytes
          read[Short](in)
          pop
        case 0x01 =>
          val (tape, register) = tapeRegister
          Read(tape, read(in)(U1Format), read(in)(U2Format), register)
        case 0x02 =>
          val (tape, register) = tapeRegister
          // drop unused byte
          read[Byte](in)
          ARead(tape, read(in)(U2Format), register)
        case 0x03 =>
          val (tape, register) = tapeRegister
          // drop unused byte
          read[Byte](in)
          SRead(tape, read(in)(U2Format), register)
        case 0x04 =>
          val (tape, _) = tapeRegister
          val write = Write(tape, read(in)(U1Format))
          // drop unused bytes
          read[Short](in)
          write
        case 0x05 =>
          val (tape, register) = tapeRegister
          // drop unused byte
          read[Byte](in)
          SWrite(tape, read(in)(U2Format), register)
        case 0x06 =>
          val (tape, _) = tapeRegister
          val movep = Movep(tape, read(in)(U1Format))
          // drop unused bytes
          read[Short](in)
          movep
        case 0x07 =>
          val (tape, _) = tapeRegister
          val movem = Movem(tape, read(in)(U1Format))
          // drop unused bytes
          read[Short](in)
          movem
        case 0x08 =>
          val (_, register) = tapeRegister
          // drop unused byte
          read[Byte](in)
          Jump(read(in)(U2Format), register)
        case 0x09 =>
          // drop unused byte
          read[Byte](in)
          val loadl = Loadl(read(in)(U1Format))
          // drop unused bytes
          read[Short](in)
          loadl
        case 0x0a =>
          val (tape, _) = tapeRegister
          val loadc = Loadc(tape)
          // drop unused bytes
          read[Byte](in)
          read[Short](in)
          loadc
        case 0x0b =>
          val (_, register) = tapeRegister
          // drop unused byte
          read[Byte](in)
          Load(read(in)(U2Format), register)
        case 0x0c =>
          val (_, register) = tapeRegister
          // drop unused byte
          read[Byte](in)
          SLoad(read(in)(U2Format), register)
        case 0x0d =>
          val (tape, _) = tapeRegister
          read(in)(TBCStringFormat) match {
            case MachineName(name, params) =>
              Call(tape, name, params)
            case parsedName =>
              throw new BytecodeFormatException("Malformed machine name " + parsedName)
          }
        case 0x0e =>
          val (_, register) = tapeRegister
          // drop unused byte
          read[Byte](in)
          Return(read(in)(U2Format), register)
        case 0x0f =>
          // drop unused bytes
          read[Int](in)
          End
        case 0x10 =>
          // drop unused bytes
          read[Short](in)
          TAlloc(read(in)(U2Format))
        case op => throw new BytecodeFormatException("Unknown opcode " + op)
      }
    }

    /*
     * opCode	tape (t)	register number (r)	length (n)	operand (d)
	 * 8 bits	4 bits	    4 bits	            8 bits  	16 bits 
     */

    def writes(out: Output, instr: Instruction) = instr match {
      case Pop(n) =>
        out.writeAll(Array(0x00, 0x00, (n & 0xff).toByte, 0x00, 0x00))
      case Read(tape, compare, offset, register) =>
        val tapeRegister = (((tape << 4) & 0xf0) + (register & 0x0f)).toByte
        out.writeAll(Array(0x01, tapeRegister, (compare & 0xff).toByte))
        write(out, (offset & 0xffff).toShort)
      case ARead(tape, offset, register) =>
        val tapeRegister = (((tape << 4) & 0xf0) + (register & 0x0f)).toByte
        out.writeByte(0x02)
        out.writeByte(tapeRegister)
        out.writeByte(0x00)
        write(out, (offset & 0xffff).toShort)
      case SRead(tape, offset, register) =>
        val tapeRegister = (((tape << 4) & 0xf0) + (register & 0x0f)).toByte
        out.writeByte(0x03)
        out.writeByte(tapeRegister)
        out.writeByte(0x00)
        write(out, (offset & 0xffff).toShort)
      case Write(tape, char) =>
        out.writeByte(0x04)
        out.writeByte(((tape << 4) & 0xf0).toByte)
        out.writeByte((char & 0xff).toByte)
        write[Short](out, 0)
      case SWrite(tape, offset, register) =>
        val tapeRegister = (((tape << 4) & 0xf0) + (register & 0x0f)).toByte
        out.writeByte(0x05)
        out.writeByte(tapeRegister)
        out.writeByte(0x00)
        write(out, (offset & 0xffff).toShort)
      case Movep(tape, of) =>
        out.writeByte(0x06)
        out.writeByte(((tape << 4) & 0xf0).toByte)
        out.writeByte((of & 0xff).toByte)
        write[Short](out, 0)
      case Movem(tape, of) =>
        out.writeByte(0x07)
        out.writeByte(((tape << 4) & 0xf0).toByte)
        out.writeByte((of & 0xff).toByte)
        write[Short](out, 0)
      case Jump(offset, register) =>
        out.writeByte(0x08)
        out.writeByte(register)
        out.writeByte(0x00)
        write(out, (offset & 0xffff).toShort)
      case Loadl(value) =>
        out.writeByte(0x09)
        out.writeByte(0x00)
        out.writeByte((value & 0xff).toByte)
        write[Short](out, 0)
      case Loadc(tape) =>
        out.writeByte(0x0a)
        out.writeByte(((tape << 4) & 0xf0).toByte)
        out.writeByte(0x00)
        write[Short](out, 0)
      case Load(offset, register) =>
        out.writeByte(0x0b)
        out.writeByte(register)
        out.writeByte(0x00)
        write(out, (offset & 0xffff).toShort)
      case SLoad(offset, register) =>
        out.writeByte(0x0c)
        out.writeByte(register)
        out.writeByte(0x00)
        write(out, (offset & 0xffff).toShort)
      case Call(tape, name, paramTypes) =>
        out.writeByte(0x0d)
        out.writeByte(((tape << 4) & 0xf0).toByte)
        val string = name + paramTypes.mkString("(", "", ");")
        out.writeAll(string.getBytes("ASCII"))
      case Return(offset, register) =>
        out.writeByte(0x0e)
        out.writeByte(register)
        out.writeByte(0x00)
        write(out, (offset & 0xffff).toShort)
      case End =>
        out.writeByte(0x0f)
        out.writeByte(0x00)
        out.writeByte(0x00)
        write[Short](out, 0)
      case TAlloc(size) =>
        out.writeByte(0x10)
        out.writeByte(0x00)
        out.writeByte(0x00)
        write(out, (size & 0xffff).toShort)
    }
  }

  implicit object BytecodeFormat extends Format[TBCFile] {

    def reads(in: Input) = {
      def readMagic =
        read(in)(U4Format) match {
          case 0x2E544243 => // OK
          case magic => // wrong magic number
            throw new BytecodeFormatException(
              "Wrong magic number! Expected: 0x2E544243, found: 0x" +
                ("%02X".format(magic)))
        }

      // read the magic number
      readMagic

      // read the version
      val version = readVersion(in)

      // read the module table
      val module_table = readModuleTable(in)

      // read the machine table
      val machine_table = readMachineTable(in)

      // build the module objects
      val modules = module_table.map {
        case (module_name, module_machines) =>
          val machines =
            module_machines.foldRight(List[Machine]()) { (mac, list) =>
              mac match {
                case (MachineName(machine_name, params), index) =>
                  Machine(machine_name, params, machine_table(index)) :: list
                case _ => list
              }
            }
          Module(module_name, machines)
      }.toList

      TBCFile(version, modules)
    }

    def writes(out: Output, value: TBCFile) = sys.error("Not implemented yet")

  }

  // helper methods and extractors

  private object MachineName {
    val regexp = """([^(]+)\((.*)\)""".r
    def unapply(name: String): Option[(String, List[Type])] =
      regexp.findFirstMatchIn(name) match {
        case Some(m) if m.groupCount == 2 =>
          val paramTypes: List[Type] = m.group(2).map {
            case 'C' => Character
            case 'S' => State
            case _ => null
          }.toList
          if (paramTypes.contains(null))
            None
          else
            Some((m.group(1), paramTypes))
        case _ => None
      }
  }

  private def readVersion(in: Input) = {
    val major = read(in)(U1Format)
    val minor = read(in)(U1Format)
    major + "." + minor
  }

  private def readModuleTable(in: Input) = {
    val modules_count = read(in)(U4Format)
    (1 to modules_count).map { _ =>
      val module_name = read(in)(TBCStringFormat)
      val module_machine_count = read(in)(U4Format)
      val machines = (1 to module_machine_count).map { _ =>
        (read(in)(TBCStringFormat), read(in)(U4Format))
      }.toList
      (module_name, machines)
    }.toList
  }

  private def readMachineTable(in: Input) = {
    val machines_count = read(in)(U4Format)
    (0 until machines_count).map { _ =>
      // u4 instructions_count;
      // instruction instructions[instructions_count];
      val instructions_count = read(in)(U4Format)
      (1 to instructions_count).map { _ =>
        read[Instruction](in)
      }.toList
    }.toArray
  }

}