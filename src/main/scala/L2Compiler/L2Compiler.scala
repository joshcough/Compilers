package L2Compiler

import java.io.File
import reader.Reader
import L2AST._
import L1Compiler.FileHelper._

trait L2CodeGenerator{
  def generateCode(ast:L2):String
}

trait L2Compiler extends L2Parser with L2CodeGenerator with Liveness with Spill { 
  def stripComments(code:String) = code.split("\n").map(s => s.takeWhile(_!=';').trim).mkString(" ")
  def read(code:String): Any = new Reader().read(stripComments(code))
  def compileFile(filename:String) = compile(new File(filename).read)
  def compile(code:String): String = generateCode(parse(read(code)))
}
