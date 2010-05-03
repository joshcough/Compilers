package L2Compiler

import java.io.File
import reader.Reader
import L2AST._
import L1Compiler.L1AST._
import L1Compiler.FileHelper._

trait L2Compiler extends Reader with L2Parser with Liveness with Spill {
  def compileFile(filename:String) = compile(new File(filename).read)

  def compile(code:String): L1 = {
    val ast = parse(read(code))
  }

  def parseProgram(s:String) = parse(read(s))
  def parseInstructionListThing(s:String): List[Instruction] = parseInstructionList(read(s).asInstanceOf[List[Any]])
  def parseInstructionListThing(a:List[Any]): List[Instruction] = parseInstructionList(a)
  def inout(code:String):List[InstuctionInOutSet] = inout(parse(read(code)).main)
  def interferingVariables(code:String) = buildInterferenceSet(inout(code)).filter{
    case (x:Variable,y:Variable) => true
    case _ => false
  }
  def attemptToColor(code:String) =
    RegisterColorGraph.base.addInterference(buildInterferenceSet(inout(code))).color
  def spill(code:String):List[Instruction] = spill(Variable("x"), -4, "s_", parseInstructionListThing(code))
}
