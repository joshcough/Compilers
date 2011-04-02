package L1Compiler.Java

import L1Compiler.{L1Compiler, Runner}

object L1JavaByteCodeRunner extends Runner {

  def main(args:Array[String]){ runFile(args(0)) }

  def run(code:String, originalFileName:String): String = {
    val compiler = new L1Compiler with JavaByteCodeGenerator
    compiler.compile(code)
  }

  // later use this:
  // jasmin goo.j
  // java -cp .:../../out/production/compilers/:../../project/boot/scala-2.8.1/lib/scala-library.jar goo
}