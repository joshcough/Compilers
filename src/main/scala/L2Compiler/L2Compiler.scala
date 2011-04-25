package L2Compiler

import java.io.File

import io.FileHelper._
import io.Reader
import L2AST._

// TODO: note...after we register allocate, we know how much space we need on the stack
// then we have to do one more thing...make some space on the stack
// ex:
//(:f
//(esp -= 8)
// ((mem ebp -4) <- esi)
// ((mem ebp -8) <- edi)
// see page 119 in http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture06.pdf
trait L2Compiler extends Reader with L2Parser with Rewriter {
  def parseProgram(s:String) = parse(read(s))
  def compileFile(filename:String) = compile(new File(filename).read)
  def compile(code: String): L2 = rewrite(parseProgram(code))
  def compileToString(code:String): String = toCode(compile(code))
}

object L2CompilerExtras extends L2CompilerExtras

trait L2CompilerExtras extends L2Compiler {
  def parseListOfInstructions(s:String): List[Instruction] = parseListOfInstructions(read(s).asInstanceOf[List[Any]])
  def parseListOfInstructions(a:List[Any]): List[Instruction] = parseInstructionList(a)
  def inoutForTesting(code:String, step:Option[Int]=None):List[InstructionInOutSet] = {
    val result = inout(parseListOfInstructions(code))
    step.map(result.reverse(_)).getOrElse(result.head)
  }
}

trait Rewriter extends Spill with Liveness with Interference with L2Printer {

  def rewrite(ast: L2): L2 = {
    //println("rewriting: " + toCode(ast))
    val newL2FunctionsAndRegisterAllocations = (ast.main :: ast.funs).map(f => allocateCompletely(f))
    val l1Functions = newL2FunctionsAndRegisterAllocations.map {
      case (f, allocs) => new VariableToRegisterReplacer(allocs).replaceVarsWithRegisters(f)
    }
    // TODO: this really needs to get cleaned up better!!!!
    // ugh! strip the main label out of the main function
    // but only if its there! For L3, I'm not putting it there.
    val main =
      if(l1Functions.head.body.head == LabelDeclaration(Label("__main"))) Func(l1Functions.head.body.tail)
      else Func(l1Functions.head.body)
    L2(main, l1Functions.tail)
  }

  def allocateCompletely(f: Func): (Func, Map[Variable, Register]) = {
    //println("allocating for: " + toCode(f))
    // first, try to see if we can do allocation without any rewriting
    //println("----inoutFinalResult-----: " + inoutFinalResult(f))
    attemptAllocation(inoutFinalResult(f))._1 match {
      case Some(registerMap) => {
        //println("first attempt to color was good!")
        //println("registerMap: " + registerMap)
        (f, registerMap)
      }
      case _ => {
        //println("first attempt to color failed!")
        // then if it fails, rewrite until we can color
        def allocateCompletely(f: Func, offset: Int): ((Func, Map[Variable, Register]), Int) = {
          val inoutset = inoutFinalResult(f)
          //println(testView(inoutset))
          val alloc = attemptAllocation(inoutset)
          alloc._1 match {
            case Some(registerMap) => ((f, registerMap), offset)
            case None => {
              //println("needing to spill")
              //println("allocation was: " + alloc._2)
              val lives = liveRanges(inoutset)
              chooseSpillVar(lives) match {
                case Some(sv) =>
                  //println("spill var: " + toCode(sv))
                  val newFunction = Func(spill(sv, offset - 4, f.body))
                  //println("new function: " + toCode(newFunction))
                  allocateCompletely(newFunction, offset - 4)
                // TODO: id prefer to use Either, or Option here instead of erroring.
                case None => error("allocation impossible")
              }
            }
          }
        }

        val rw = initialRewrite(f)
        val ((almostFinalFunction, allocs), espOffset) = allocateCompletely(rw, 0)

        // adjust the stack at the start of the function right here.
        // TODO: horrible implementation
        if(espOffset == 0) (almostFinalFunction, allocs)
        else {
          val isMain = f.body.head == LabelDeclaration(Label("__main"))
          val label = almostFinalFunction.body.head
          val bodyWithoutLabel = almostFinalFunction.body.drop(1)
          val decEsp = List(Decrement(esp, Num(- espOffset)))
          val incEspMaybe = if(isMain) List(Increment(esp, Num(- espOffset))) else List()
          val newBody = label :: decEsp ::: bodyWithoutLabel ::: incEspMaybe
          (Func(newBody), allocs)
        }
      }
    }
  }

  def initialRewrite(f:Func): Func = {
    val z1In = Assignment(Variable("__z1"), edi)
    val z2In = Assignment(Variable("__z2"), esi)
    val z1Out = Assignment(edi, Variable("__z1"))
    val z2Out = Assignment(esi, Variable("__z2"))
    Func(f.body.head :: List(z1In,z2In) ::: f.body.drop(1).flatMap {
      i => i match {
        case Return => List(z1Out,z2Out, Return)
        case t:TailCall => List(z1Out,z2Out, t)
        case _ => List(i)
      }
    })
  }

  def chooseSpillVar(liveRanges: List[List[LiveRange]]): Option[Variable] = {
    def maxRange(ranges:List[LiveRange]): Option[LiveRange] = ranges match {
      case Nil => None
      case _ => Some(ranges.sortWith(_.range > _.range).head)
    }
    liveRanges.flatMap(maxRange).sortWith{_.range > _.range}.map(_.x)
            .collect{case v:Variable => v}.filterNot(_.name.startsWith("spilled_var")).headOption
  }

  class VariableToRegisterReplacer(replacements:Map[Variable, Register]) {

    def replaceVarsWithRegisters(f:Func): Func = Func(f.body.map(replaceVarsWithRegisters))

    private def getRegister(x:X): Register = x match {
      case v:Variable => replacements(v)
      case r:Register => r
    }

    def replaceVarsWithRegisters(i:Instruction): Instruction = i match {
      case Assignment(x1, x2) => Assignment(getRegister(x1), replaceVarsWithRegisters(x2))
      case Increment(s1, s2) => Increment(getRegister(s1), replaceVarsWithRegisters(s2))
      case Decrement(s1, s2) => Decrement(getRegister(s1), replaceVarsWithRegisters(s2))
      case Multiply(s1, s2) => Multiply(getRegister(s1), replaceVarsWithRegisters(s2))
      case BitwiseAnd(s1, s2) => BitwiseAnd(getRegister(s1), replaceVarsWithRegisters(s2))
      case LeftShift(s1, s2) => LeftShift(getRegister(s1), replaceVarsWithRegisters(s2))
      case RightShift(s1, s2) => RightShift(getRegister(s1), replaceVarsWithRegisters(s2))
      case MemWrite(loc, x) => MemWrite(replaceVarsWithRegisters(loc), replaceVarsWithRegisters(x))
      case g:Goto => g
      case CJump(comp, l1, l2) => CJump(replaceVarsWithRegisters(comp), l1, l2)
      case Call(x) => Call(replaceVarsWithRegisters(x))
      case TailCall(x) => TailCall(replaceVarsWithRegisters(x))
      case ld:LabelDeclaration => ld
      case Return => Return
    }

    def replaceVarsWithRegisters(s:S): S = s match {
      case r:Register => r
      case v:Variable => getRegister(v)
      case n:Num => n
      case l:Label => l
    }

    def replaceVarsWithRegisters(rhs:AssignmentRHS):AssignmentRHS = rhs match {
      case MemRead(loc) => MemRead(replaceVarsWithRegisters(loc))
      case Print(s) => Print(replaceVarsWithRegisters(s))
      case Allocate(n, init) => Allocate(replaceVarsWithRegisters(n), replaceVarsWithRegisters(init))
      case ArrayError(a, n) => ArrayError(replaceVarsWithRegisters(a), replaceVarsWithRegisters(n))
      case c:Comp => replaceVarsWithRegisters(c)
      case s:S => replaceVarsWithRegisters(s)
    }

    private def replaceVarsWithRegisters(loc:MemLoc): MemLoc =
      MemLoc(getRegister(loc.basePointer),loc.offset)
    private def replaceVarsWithRegisters(c:Comp): Comp =
      Comp(replaceVarsWithRegisters(c.s1), c.op, replaceVarsWithRegisters(c.s2))
  }
}