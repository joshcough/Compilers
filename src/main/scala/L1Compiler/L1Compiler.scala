package L1Compiler

import java.io.File
import L1AST._
import io.{CommandRunner, Reader}
import io.FileHelper._
import io.Dir._

trait L1Compiler extends Reader with L1Parser with BackEnd {
  def compile(code:String, unitName:String): String = generateCode(parse(read(code)), unitName)
}

trait BackEnd{
  def generateCode(ast:L1, originalFileName:String):String
}

trait Runner{
  def runFile(file:File): String = run(file.read, file.getAbsolutePath)
  def run(code:String, originalFileName:String): String
  def test(code:String) = run(code, "Test.L1")
}
