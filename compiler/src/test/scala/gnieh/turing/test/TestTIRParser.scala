package gnieh.turing
package test

import compiler.tir._

import java.io.FileReader

object TestTIRParser extends App {

  import TIRParser._

  parseAll(tirMachine, new FileReader("src/test/resources/tir/not.tir")) match {
    case Success(parsed, _) => println(parsed)
    case f => println(f)
  }

}