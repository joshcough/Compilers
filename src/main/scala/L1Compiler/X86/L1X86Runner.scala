package L1Compiler.X86

import L1Compiler.{L1Compiler, Runner}
import java.io.File
import io.FileHelper
import io.FileHelper._
import io.CommandRunner._

object L1X86Runner extends Runner {

  def main(args:Array[String]){
    compileForRun(new File(args(0)).read, args(0))
  }

  def compileForRun(code:String, originalFileName:String): String = {
    val compiler = new L1Compiler with X86Generator
    val outputAssemblyFile = originalFileName.dropRight(3) + ".S"
    val outputOFile = originalFileName.dropRight(3) + ".o"
    new File(outputAssemblyFile).write(compiler.compile(code, originalFileName))
    runAndDieOneErrors("gcc -O2 -c -o ./runtime.o ./src/main/compilers/L1/runtime.c")
    runAndDieOneErrors("as -o " + outputOFile + " " + outputAssemblyFile)
    runAndDieOneErrors("gcc -o ./a.out "+outputOFile+" runtime.o")
  }

  def run(code:String, originalFileName:String): String = {
    compileForRun(code, originalFileName)
    runAndDieOneErrors("./a.out")
  }
}