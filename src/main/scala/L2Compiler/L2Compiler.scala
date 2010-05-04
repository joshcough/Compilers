package L2Compiler

import java.io.File
import reader.Reader
import L2AST._
import L1Compiler.L1AST._
import L1Compiler.FileHelper._

trait L2Compiler extends Reader with L2Parser with Liveness with Spill {
  def parseProgram(s:String) = parse(read(s))
  def compileFile(filename:String) = compile(new File(filename).read)
  def compile(code: String): L1 = {
    def color(f:Func) = RegisterColorGraph.base.addInterference(buildInterferenceSet(inout(f))).color

    def colorCompletely(f: Func): (Func, RegisterColorGraph) = {
      def colorCompletely(f: Func, offset: Int): (Func, RegisterColorGraph) = {
        color(f) match {
          case Some(coloring) => (f, coloring)
          case None => {
            colorCompletely(Func(f.name,
              spill(chooseSpillVar(liveRanges(inout(f))).get, offset, "spilled_var_", f.body)), offset - 4)
          }
        }
      }
      colorCompletely(f, -4)
    }
    val ast = parseProgram(code)
    val funsAndColors = (ast.main :: ast.funs).map(colorCompletely)
    val elOneFunctions = funsAndColors.map {
      case (func, colorGraph) => colorGraph.replaceVarsWithRegisters(func)
    }
    L1(elOneFunctions.head, elOneFunctions.tail)
  }


  // these are testing entry points...
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
