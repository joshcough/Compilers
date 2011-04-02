package L1Compiler.X86

import L1Compiler.CommandRunner._
import java.io.File
import L1Compiler.{FileHelper, L1Compiler, Runner}
import FileHelper._

object L1X86Runner extends Runner {

  def main(args:Array[String]){ runFile(args(0)) }

  def run(code:String, originalFileName:String): String = {
    val compiler = new L1Compiler with X86Generator
    val outputAssemblyFile = originalFileName.dropRight(3) + ".S"
    val outputOFile = originalFileName.dropRight(3) + ".o"
    new File(outputAssemblyFile).write(compiler.compile(code))
    runAndDieOneErrors("gcc -O2 -c -o ./runtime.o ./src/main/compilers/L1/runtime.c")
    runAndDieOneErrors("as -o " + outputOFile + " " + outputAssemblyFile)
    runAndDieOneErrors("gcc -o ./a.out "+outputOFile+" runtime.o")
    runAndDieOneErrors("./a.out")
  }
}