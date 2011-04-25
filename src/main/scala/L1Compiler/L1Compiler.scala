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
//
//object L1Interpreter extends Runner {
//  def run(code:String, originalFileName:String): String = {
//    val (out, err) = CommandRunner("./src/test/compilers/interpreters/L1" + " " + new File(originalFileName).getAbsolutePath)
//    if(! (err startsWith "Welcome to L1")) error("interpreter died with the following errors:\n" + err)
//    val resultFile = new File(originalFileName.dropRight(3) + ".res")
//    resultFile.write(out)
//    out
//  }
//}