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
package gnieh.turing.tree

/**
 * @author Lucas Satabin
 *
 */
sealed trait Node

/** This class represents an identifier with a given name */
final case class Ident(name: String) extends Node {
  override def toString = name
}

final case class CompilationUnit(module: Option[Ident],
                                 uses: List[Ident],
                                 machines: List[Machine]) extends Node {
  override def toString = {
    val mod = module match {
      case Some(m) => "module " + m + "\n\n"
      case _ => ""
    }

    val u = uses.map(u => "use " + u).mkString("", "\n", "\n")

    val macs = machines.mkString("\n\n")

    mod + u + macs
  }
}

final case class Var(name: Ident, tpe: Option[Type] = None) extends Node {
  override def toString = tpe match {
    case Some(t) => name + ": " + t
    case _ => name.toString
  }
}

final case class Machine(name: Ident,
                         param: List[Var],
                         tapes: List[Var] = Nil,
                         transitions: List[Transition] = Nil,
                         oracle: Boolean = true) extends Node {
  override def toString = {
    val or = if (oracle)
      "orcale"
    else
      ""
    val tap = tapes.mkString("\n", "\n", "\n")

    val trans = transitions.mkString("\n")

    or + name + param.mkString("(", ", ", ")") + tap + trans + "\n--"
  }
}

final case class Transition(initial: Option[InitialState],
                            read: Read,
                            actions: List[Action],
                            next: Next) extends Node {
  override def toString =
    initial.getOrElse(" ") + " | " + read + " | " + actions.mkString(" ") +
      " | " + next
}

sealed trait InitialState extends Node
final case class Decl(name: Ident, params: List[Var]) extends InitialState {
  override def toString =
    name + params.mkString("(", ", ", ")")
}
final case class Named(name: Ident) extends InitialState {
  override def toString = name.toString
}

sealed trait Read extends Node {
  val tape: Option[Ident]
}
final case class AnyChar(local: Option[Var], tape: Option[Ident]) extends Read {
  override def toString = {
    val l = local match {
      case Some(l) => l + " <- "
      case _ => ""
    }

    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    l + tap + "any"
  }
}
final case class AllChar(local: Option[Var], tape: Option[Ident]) extends Read {
  override def toString = {
    val l = local match {
      case Some(l) => l + " <- "
      case _ => ""
    }

    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    l + tap + "all"
  }
}
final case class NoneChar(tape: Option[Ident]) extends Read {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    tap + "none"
  }
}
final case class SingleChar(tape: Option[Ident], char: Char) extends Read {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    tap + "'" + char + "'"
  }
}

final case class IdentRead(tape: Option[Ident], name: Ident) extends Read {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    tap + name
  }
}

sealed trait Action extends Node {
  val tape: Option[Ident]
}
final case class Del(tape: Option[Ident]) extends Action {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    tap + "del"
  }
}
final case class WriteChar(tape: Option[Ident], char: Char) extends Action {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }
    tap + "write '" + char + "'"
  }
}
final case class WriteString(tape: Option[Ident], str: String) extends Action {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    tap + "write \"" + str + "\""
  }
}
final case class WriteVar(tape: Option[Ident], name: Ident) extends Action {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }

    tap + "write " + name
  }
}
final case class Left(tape: Option[Ident], offset: Int) extends Action {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }
    tap + "left " + offset
  }
}
final case class Right(tape: Option[Ident], offset: Int) extends Action {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }
    tap + "right " + offset
  }
}

sealed trait Next extends Node with Arg
case object End extends Next {
  override def toString = "end"
}
final case class NextIdent(name: Ident) extends Next {
  override def toString = name.toString
}
final case class NextCall(tape: Option[Ident], name: Ident, args: List[Arg])
    extends Next {
  override def toString = {
    val tap = tape match {
      case Some(t) => t + "."
      case _ => ""
    }
    tap + name + args.mkString("(", ", ", ")")
  }
}

sealed trait Arg extends Node
final case class CharArg(char: Char) extends Arg {
  override def toString = "'" + char + "'"
}