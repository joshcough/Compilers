package L2Compiler

import io.Reader
import L2AST._
import util.Timer

object L2CompilerMain extends L2Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = toCode(compile(new File(filename).read))
}

trait L2Compiler extends Reader with L2Parser with Allocator with L2Printer with Timer {
  def parseProgram(s:String): L2 = parse(read(s))
  def compile(code: String): L2 = compile(parseProgram(code))
  def compile(ast:L2): L2 = {
    val l1 = timed("allocate", allocate(ast))
    val mainWithoutLabel = l1.main.body.headOption match {
      case Some(l) if l == mainLabel => Func(l1.main.body.tail)
      case _ => l1.main
    }
    L2(mainWithoutLabel,l1.funs)
  }

  def compileToString(code:String): String = toCode(compile(code))

  // extras, testing related stuff
  def parseListOfInstructions(s:String): List[Instruction] = parseListOfInstructions(read(s).asInstanceOf[List[Any]])
  def parseListOfInstructions(a:List[Any]): List[Instruction] = parseInstructionList(a)
  def inoutForTesting(code:String, step:Option[Int]=None):List[InstructionInOutSet] = {
    val result = inout(parseListOfInstructions(code))
    // dont remove this. its for printing out all the steps in liveness.
    //result.reverse.map(L2Printer.testView).foreach(x => println(x + "\n---------\n"))
    step.map(result.reverse(_)).getOrElse(result.head)
  }
}