package L2Compiler

import java.io.File

import io.FileHelper._
import io.Reader
import L2AST._

object L2CompilerMain extends L2CompilerExtras {
  def main(args:Array[String]) = {
    
  }
}

trait L2Compiler extends Reader with L2Parser with Allocator {
  def parseProgram(s:String) = parse(read(s))
  def compileFile(filename:String) = compile(new File(filename).read)
  def compile(code: String): L2 = {
    val l2 = allocate(parseProgram(code))
    val mainWithoutLabel = Func(l2.main.body.tail)
    L2(mainWithoutLabel, l2.funs)
  }
  def compileToString(code:String): String = toCode(compile(code))
}

object L2CompilerExtras extends L2CompilerExtras

trait L2CompilerExtras extends L2Compiler {
  def parseListOfInstructions(s:String): List[Instruction] = parseListOfInstructions(read(s).asInstanceOf[List[Any]])
  def parseListOfInstructions(a:List[Any]): List[Instruction] = parseInstructionList(a)
  def inoutForTesting(code:String, step:Option[Int]=None):List[InstructionInOutSet] = {
    val result = inout(parseListOfInstructions(code))
    // dont remove this. its for printing out all the steps in liveness.
    //result.reverse.map(L2Printer.testView).foreach(x => println(x + "\n---------\n"))
    step.map(result.reverse(_)).getOrElse(result.head)
  }
}
