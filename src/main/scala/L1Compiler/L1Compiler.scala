package L1Compiler

import java.io.File
import L1AST._
import io.{CommandRunner, Reader}
import io.FileHelper._

object Dir {
  val L1 = "./src/main/compilers/L1/"
  def testFiles = new File(L1+ "/1-test").list.toList.filter(_.endsWith("L1"))
  def L1File(name:String) = Dir.L1 + name
}

import Dir._

trait L1Compiler extends Reader with L1Parser with BackEnd {
  def compile(code:String, unitName:String): String = generateCode(parse(read(code)), unitName)
}

trait BackEnd{
  def generateCode(ast:L1, originalFileName:String):String
}

trait Runner{
  def runFile(filename:String): String = run(new File(filename).read, filename)
  def run(code:String, originalFileName:String): String
  def test(code:String) = run(code, "Test.L1")
}

object L1Interpreter extends Runner {
  def run(code:String, originalFileName:String): String = {
    val (out, err) = CommandRunner("./src/main/compilers/interpreters/L1" + " " + new File(originalFileName).getAbsolutePath)
    if(err != "Welcome to L1, v17") error("interpreter died with the following errors:\n" + err)
    val resultFile = new File(originalFileName.dropRight(3) + ".res")
    resultFile.write(out)
    out
  }
}