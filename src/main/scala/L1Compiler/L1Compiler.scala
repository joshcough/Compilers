package L1Compiler

import java.io.File
import reader.Reader
import L1AST._
import CommandRunner._
import FileHelper._

object Dir {
  val L1 = "./src/main/compilers/L1/"
  def testFiles = new File(L1+ "/code").list.toList.filter(_.endsWith("L1"))
  def L1File(name:String) = Dir.L1 + name
}

import Dir._

trait L1Compiler extends Reader with L1Parser with BackEnd {
  def compile(code:String): String = generateCode(parse(read(code)))
}

trait BackEnd{
  def generateCode(ast:L1):String
}

trait Runner{
  def runFile(filename:String): String = run(new File(filename).read, filename)
  def run(code:String, originalFileName:String): String
  def test(code:String) = run(code, "Test.L1")
}

object L1Interpreter extends Runner {
  def run(code:String, originalFileName:String): String = {
    val (out, err) = CommandRunner(L1File("L1") + " " + new File(originalFileName).getAbsolutePath)
    if(err != "Welcome to L1, v7") error("interpreter died with the following errors:\n" + err)
    val resultFile = new File(originalFileName.dropRight(3) + ".res")
    resultFile.write(out)
    out
  }
}