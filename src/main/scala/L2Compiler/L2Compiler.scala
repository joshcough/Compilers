package L2Compiler

import java.io.File
import reader.Reader
import L2AST._
import L1Compiler.FileHelper._

// TODO: note...after we register allocate, we know how much space we need on the stack
// then we have to do one more thing...make some space on the stack
// ex:
//(:f
//(esp -= 8)
// ((mem ebp -4) <- esi)
// ((mem ebp -8) <- edi)
// see page 119 in http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf

trait L2Compiler extends Reader with L2Parser with Liveness with Spill {
  def parseProgram(s:String) = parse(read(s))
  def compileFile(filename:String) = compile(new File(filename).read)
  def compile(code: String): L2 = {
    def color(f:Func) = RegisterColorGraph.base.addInterference(buildInterferenceSet(inout(f))).color

    def initialRewrite(f:Func) = {
      val z1In = Assignment(Variable("__z1"), ebx) // TODO: this one probably has to be removed
      val z2In = Assignment(Variable("__z2"), edi)
      val z3In = Assignment(Variable("__z3"), esi)
      val z1Out = Assignment(ebx, Variable("__z1"))  // TODO: this one probably has to be removed
      val z2Out = Assignment(edi, Variable("__z2"))
      val z3Out = Assignment(esi, Variable("__z3"))
      Func(f.name, List(z1In,z2In,z3In) ::: f.body ::: List(z1Out,z2Out,z3Out))
    }

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
    val funsAndColors = (ast.main :: ast.funs).map(f => colorCompletely(initialRewrite(f)))
    val elOneFunctions = funsAndColors.map {
      case (func, colorGraph) => {
        //println("color graph: " + colorGraph)
        colorGraph.replaceVarsWithRegisters(func)
      }
    }
    L2(elOneFunctions.head, elOneFunctions.tail)
  }


  // these are testing entry points...
  def parseListOfInstructions(s:String): List[Instruction] = parseListOfInstructions(read(s).asInstanceOf[List[Any]])
  def parseListOfInstructions(a:List[Any]): List[Instruction] = parseInstructionList(a)
  def inoutForTesting(code:String, stopAfterNSteps:Option[Int]=None):String = //List[InstuctionInOutSet] =
    inout(parseFunction(read(code)), stopAfterNSteps).mkString("\n")
//  def interferingVariables(code:String) = buildInterferenceSet(inout(code, None)).filter{
//    case (x:Variable,y:Variable) => true
//    case _ => false
//  }
//  def attemptToColor(code:String) =
//    RegisterColorGraph.base.addInterference(buildInterferenceSet(inout(code, None))).color
  def spill(code:String):List[Instruction] = spill(Variable("x"), -4, "s_", parseListOfInstructions(code))

}
