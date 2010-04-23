package L1Compiler.X86

import L1Compiler.CommandRunner._
import L1Compiler.{L1Compiler, Runner}

object L1X86Runner extends Runner {

  def main(args:Array[String]){
    run(args(0))
  }

  def run(filename:String) = {
    val compiler = new L1Compiler with L1X86Generator
    compiler.compileFile(filename)
    runAndDieOneErrors("./a.out")
  }

  def compileAndRunCode(code:String): String = {
    val compiler = new L1Compiler with L1X86Generator
    compiler.compileCodeAndWriteOutput(code, "test.L1")
    runAndDieOneErrors("./a.out")
  }
}