package L2Compiler

import java.io.File
import reader.Reader
import L2AST._
import L1Compiler.L1AST._
import L1Compiler.FileHelper._

trait L2CodeGenerator{
  def generateCode(ast:L2):String
}

trait L2Compiler extends Reader with L2Parser with L2CodeGenerator with Liveness with Spill {
  def compileFile(filename:String) = compile(new File(filename).read)
  def compile(code:String): String = generateCode(parse(read(code)))

  def parseProgram(s:String) = parse(read(s))
  def parseInstructionListThing(s:String): List[Instruction] = parseInstructionList(read(s).asInstanceOf[List[Any]])
  def parseInstructionListThing(a:List[Any]): List[Instruction] = parseInstructionList(a)
  def inout(code:String) = inoutHack(parseInstructionListThing(code))
  def inoutHack(ins:List[Instruction]): List[InstuctionInOutSet] = {
    inout(ins.map(InstuctionInOutSet(_, Set[X](), Set[X]())))
  }
  def interferingVariables(code:String) = buildInterferenceSet(inout(code)).filter{
    case (x:Variable,y:Variable) => true
    case _ => false
  }
  def attemptToColor(code:String) = RegisterColorGraph.base.addInterference(buildInterferenceSet(inout(code))).color
  def spill(code:String):List[Instruction] = spill(Variable("x"), -4, "s_", parseInstructionListThing(code))

}
