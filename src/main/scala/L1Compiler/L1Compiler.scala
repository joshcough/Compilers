package L1Compiler

import java.io.File
import L1AST._
import io.{CommandRunner, Reader}
import io.FileHelper._
import io.Dir._
import X86.X86Generator

trait L1Compiler extends Reader with L1Parser with BackEnd {
  def compile(code:String, unitName:String): String = generateCode(parse(read(code)), unitName)
}

object L1CompilerMain extends L1Compiler with X86Generator {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = compile(new File(filename).read, "test")
}

trait BackEnd{
  def generateCode(ast:L1, originalFileName:String):String
}

trait Runner{
  def runFile(file:File): String = run(file.read, file.getAbsolutePath)
  def run(code:String, originalFileName:String): String
  def test(code:String) = run(code, "Test.L1")
}
