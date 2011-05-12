package L1Compiler

import java.io.File
import io.Reader
import io.FileHelper._
import io.Dir._
import L2Compiler.{L2Parser, L2AST}
import L2AST.{L2 => L1}

trait L1Compiler extends Reader with L2Parser with BackEnd {
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
