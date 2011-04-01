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

  def compileFile(filename:String) {
    compileCodeAndWriteOutput(new File(filename).read, filename)
  }

  // just return the assembly code result
  def compile(code:String): String = generateCode(parse(read(code)))

  def compileCodeAndWriteOutput(code:String, originalL1Filename:String){
    val compiledCode = compile(code)
    //compiledCode.split("\n").zipWithIndex.foreach{ case (c,i) => println(i + ":\t" + c) }
    val outputAssemblyFile = originalL1Filename.dropRight(3) + ".S"
    val outputOFile = originalL1Filename.dropRight(3) + ".o"
    new File(outputAssemblyFile).write(compiledCode)
    runAndDieOneErrors("gcc -O2 -c -o ./runtime.o ./src/main/compilers/L1/runtime.c")
    runAndDieOneErrors("as -o " + outputOFile + " " + outputAssemblyFile)
    runAndDieOneErrors("gcc -o ./a.out "+outputOFile+" runtime.o")
  }

}

trait BackEnd{
  def generateCode(ast:L1):String
}

trait Runner{
  def run(filename:String): String
  def runString(code:String): String = ""
}

object L1Interpreter extends Runner {
  def run(filename:String) = {
    val codeFile = new File(filename).getAbsolutePath
    val (out, err) = CommandRunner(L1File("L1") + " " + codeFile)
    if(err != "Welcome to L1, v7") error("interpreter died with the following errors:\n" + err)
    val resultFile = new File(filename.dropRight(3) + ".res")
    resultFile.write(out)
    out
  }
}