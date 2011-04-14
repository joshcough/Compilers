package L2Compiler

import java.io.File
import io.Reader
import L2AST._
import io.FileHelper._
import io.Reader

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
    def color(f:Func) = {
      val interference = buildInterferenceSet(inoutFinalResult(f))
      RegisterColorGraph.base.addInterference(interference).color
    }

    def initialRewrite(f:Func) = {
      val z1In = Assignment(Variable("__z1"), edi)
      val z2In = Assignment(Variable("__z2"), esi)
      val z1Out = Assignment(edi, Variable("__z1"))
      val z2Out = Assignment(esi, Variable("__z2"))
      Func(f.body.head :: List(z1In,z2In) ::: f.body.drop(1) ::: List(z1Out,z2Out))
    }

    def colorCompletely(f: Func): (Func, RegisterColorGraph) = {
      def colorCompletely(f: Func, offset: Int): (Func, RegisterColorGraph) = {
        color(f) match {
          case Some(coloring) => (f, coloring)
          case None => {
            colorCompletely(Func(spill(
              chooseSpillVar(liveRanges(inoutFinalResult(f))).get, offset, "spilled_var_", f.body)), offset - 4)
          }
        }
      }
      colorCompletely(f, -4)
    }
    val ast = parseProgram(code)
    val funsAndColors = (ast.main :: ast.funs).map(f => colorCompletely(initialRewrite(f)))
    val l1OneFunctions = funsAndColors.map {
      case (func, colorGraph) => {
        //println("color graph: " + colorGraph)
        colorGraph.replaceVarsWithRegisters(func)
      }
    }
    L2(l1OneFunctions.head, l1OneFunctions.tail)
  }

  def chooseSpillVar(liveRanges: List[List[LiveRange]]): Option[Variable] = {
    def maxRange(ranges:List[LiveRange]): Option[LiveRange] = ranges match {
      case Nil => None
      case _ => Some(ranges.sortWith(_.range > _.range).head)
    }
    liveRanges.flatMap(maxRange).sortWith{_.range > _.range}.
            find(_.x.isInstanceOf[Variable]).map(_.x.asInstanceOf[Variable])
  }

  // these are testing entry points...
  def parseListOfInstructions(s:String): List[Instruction] = parseListOfInstructions(read(s).asInstanceOf[List[Any]])
  def parseListOfInstructions(a:List[Any]): List[Instruction] = parseInstructionList(a)
  def inoutForTesting(code:String, step:Option[Int]=None):List[InstructionInOutSet] = {
    val result = inout(parseListOfInstructions(code))
    step.map(result.reverse(_)).getOrElse(result.head)
  }

  // TODO: variables Registers can interfere.
  // see http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture05.pdf p19
  // basically, if they are alive at the same time...they interfere.
  // hmm...better read moreabout this. see page 18 also
  def interferingVariables(code:String) = buildInterferenceSet(inoutForTesting(code, None)).filter{
    case (x:Variable,y:Variable) => true
    case _ => false
  }
//  def attemptToColor(code:String) =
//    RegisterColorGraph.base.addInterference(buildInterferenceSet(inout(code, None))).color
  def spill(code:String):List[Instruction] = spill(Variable("x"), -4, "s_", parseListOfInstructions(code))

}