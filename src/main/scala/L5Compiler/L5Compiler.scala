package L5Compiler

import L5AST._

object L5CompilerMain extends L5Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = compileToString(new File(filename).read)
}

trait L5Compiler extends io.Reader with L5Parser with L5Printer {

  def compile(code:String): E = {
    val ast = parse(read(code))
    ast
  }

  def compileToString(code:String) = L5Printer.toCode(compile(code))
}