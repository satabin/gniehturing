package gnieh.turing
package test

import compiler._

object TestSymbolTableBuilder extends App {

  GniehTuringC.main(Array(
    "src/test/resources/tmdl/simple.tmdl",
    "-v"))

}