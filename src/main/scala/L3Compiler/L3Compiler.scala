package L3Compiler

import L4Compiler.L4AST._
import L2Compiler.L2AST.{
  Func => L2Func,
  LessThan => L2LessThan,
  LessThanOrEqualTo => L2LessThanOrEqualTo,
  EqualTo => L2EqualTo,
  Num => L2Num,
  Variable => L2Variable,
  X => L2X,
  Label => L2Label,
  Print => L2Print,
  Main => L2Main,
  Instruction => L2Instruction, _}
import L2Compiler.L2Printer

object L3CompilerMain extends L3Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = L2Printer.toCode(compile(new File(filename).read))
}

class L3Compiler extends io.Reader with L3Parser with L3ToL2Implicits {

  def compileToString(code:String): String = L2Printer.toCode(compile(code))

  def compile(code:String): L2 = compile(parse(read(code)))

  def compile(p:L3): L2 = p match {
    case L3(main, funcs) => L2(
      main=L2Func(List(Call(mainLabel))),
      funs = L2Main(compileE(main)) :: (funcs map compileFunction))
  }

  // (l (x ...) e)
  def compileFunction(f:Func): L2Func = {
    val argAssignments =
        for( (v, r) <- f.args.zip(List(ecx, edx, eax))) yield Assignment(v, r)
    L2Func((declare(f.label) :: argAssignments ::: compileE(f.body)) :+ Return)
  }

  def compileE(e:E): List[L2Instruction] = e match {
    // e ::= (let ([x d]) e) | (if v e e) | d
    case Let(v:Variable, d:D, body:E) => compileD(d=d, destination=v) ::: compileE(body)
    case IfStatement(v:V, t:E, f:E) => {
      val tmp = temp()
      val thenLabel = tempLabel()
      val elseLabel = tempLabel()
      List(
        compileD(v, tmp),
        List(CJump(Comp(tmp, L2EqualTo, Num(1)), elseLabel, thenLabel)),
        List(LabelDeclaration(thenLabel)),
        compileE(t),
        List(LabelDeclaration(elseLabel)),
        compileE(f)).flatten
    }

    //3) the e is a d:
    //-> if it is an application, make a tail call
    //   otherwise, generate the code for the d,
    //   store the result in eax, and return.
    // TODO: something is funny about passing eax and tailcall=true...
    // it seems like i can collapse that into one argument somehow.
    // because eax sort of signals that a tailcall should happen.
    // otherwise we would get a variable and we need to store
    // that variable into eax...hmmm...
    case f:FunCall => compileFunCall(f, eax, tailCall = true)
    case d:D => compileD(d, eax) ::: List(Return)
  }

  def encode(v:V): S = v match {
    case Num(n) => L2Num(n*2+1)
    case v:Variable => convertVar(v)
    case Label(name) => L2Label(name)
  }

  def compileFunCall(f:FunCall, destination:L2X, tailCall:Boolean): List[L2Instruction] = {
    val argAssignments =
      for( (r, v) <- List(ecx, edx, eax).zip(f.args map encode) ) yield Assignment(r, v)
    argAssignments ::: (
      if(tailCall) List(TailCall(f.v))
      else List(Call(f.v), Assignment(destination, eax))
    )
  }

  def compileD(d:D, destination: L2X): List[L2Instruction] = d match {
    case Print(v) => List(Assignment(eax, L2Print(encode(v))), Assignment(destination, eax))

    // TODO...tail-call if last d in the tree??
    case f:FunCall => compileFunCall(f, destination, tailCall = false)

    //biop ::= + | - | * | < | <= | =
    case Add(l:V, r:V)  => List(
      Assignment(destination, encode(l)),
      Increment(destination, encode(r)),
      Decrement(destination, Num(1)))

    case Sub(l:V, r:V)  => List(
      Assignment(destination, encode(l)),
      Decrement(destination, encode(r)),
      Increment(destination, Num(1)))

    case Mult(l:V, r:V)  => {
      val tmp = temp()
      List(
        Assignment(tmp, encode(l)),
        RightShift(tmp, Num(1)),
        Assignment(destination, encode(r)),
        RightShift(destination, Num(1)),
        Multiply(destination, tmp),
        LeftShift(destination, Num(1)),
        Increment(destination, Num(1)))
    }

    case LessThan(l:V, r:V) => List(
      Assignment(destination, encode(l)),
      Assignment(destination, Comp(destination, L2LessThan, encode(r))),
      LeftShift(destination, Num(1)),
      Increment(destination, Num(1)))

    case LessThanOrEqualTo(l:V, r:V)  => List(
      Assignment(destination, encode(l)),
      Assignment(destination, Comp(destination, L2LessThanOrEqualTo, encode(r))),
      LeftShift(destination, Num(1)),
      Increment(destination, Num(1)))

    case EqualTo(l:V, r:V)  => List(
      Assignment(destination, encode(l)),
      Assignment(destination, Comp(destination, L2EqualTo, encode(r))),
      LeftShift(destination, Num(1)),
      Increment(destination, Num(1)))

    case IsNumber(v:V) => List(
      Assignment(destination, encode(v)),
      BitwiseAnd(destination, Num(1)),
      LeftShift(destination, Num(1)),
      Increment(destination, Num(1))
    )

    case IsArray(v:V) => List(
      Assignment(destination, encode(v)),
      BitwiseAnd(destination, Num(1)),
      Multiply(destination, Num(-2)),
      Increment(destination, Num(3))
    )
      
    case NewArray(size:V, init:V) => List(
      Assignment(eax, Allocate(encode(size), encode(init))),
      Assignment(destination, eax)
    )

    // TODO: review assertions, particularly x vs variable
    // TODO...does this loc definitly have to be a num????
    //(x <- (mem s n4))
    case ARef(arr:V, loc:V) => {
      assert(arr.isInstanceOf[Variable])
      assert(loc.isInstanceOf[Num])
      val index = destination
      val size = temp()
      val boundsFailLabel = tempLabel()
      val boundsPassLabel = tempLabel()
      val checkNegativeLabel = tempLabel()

      List(
        Assignment(index, encode(loc.asInstanceOf[Num])),
        RightShift(index, Num(1)),
        Assignment(size, MemRead(MemLoc(convertVar(arr.asInstanceOf[Variable]), Num(0)))),
        CJump(Comp(size, L2LessThanOrEqualTo, index), boundsFailLabel, checkNegativeLabel),
        LabelDeclaration(boundsFailLabel),
        LeftShift(index, Num(1)),
        Increment(index, Num(1)),
        Assignment(eax, ArrayError(arr, index)),
        LabelDeclaration(checkNegativeLabel),
        CJump(Comp(index, L2LessThan, Num(0)), boundsFailLabel, boundsPassLabel),
        LabelDeclaration(boundsPassLabel),
        Increment(index, Num(1)),
        Multiply(index, Num(4)),
        Increment(index, arr),
        Assignment(destination, MemRead(MemLoc(index, Num(0))))
      )
    }

    case ALen(arr:V) => List(
      Assignment(destination, MemRead(MemLoc(convertVar(arr.asInstanceOf[Variable]), Num(0)))),
      LeftShift(destination, Num(1)),
      Increment(destination, Num(1))
    )

    //((mem x n4) <- s)
    //(let ([x (aset v1 v2 v3)]) ...)
    case ASet(arr:V, loc:V, newVal: V) => {
      assert(arr.isInstanceOf[Variable])
      assert(loc.isInstanceOf[Num])
      val index = destination
      val size = temp()
      val boundsFailLabel = tempLabel()
      val boundsPassLabel = tempLabel()
      val checkNegativeLabel = tempLabel()

      List(
        Assignment(index, encode(loc.asInstanceOf[Num])),
        RightShift(index, Num(1)),
        Assignment(size, MemRead(MemLoc(convertVar(arr.asInstanceOf[Variable]), Num(0)))),
        CJump(Comp(size, L2LessThanOrEqualTo, index), boundsFailLabel, checkNegativeLabel),
        LabelDeclaration(boundsFailLabel),
        LeftShift(index, Num(1)),
        Increment(index, Num(1)),
        Assignment(eax, ArrayError(arr, index)),
        LabelDeclaration(checkNegativeLabel),
        CJump(Comp(index, L2LessThan, Num(0)), boundsFailLabel, boundsPassLabel),
        LabelDeclaration(boundsPassLabel),
        Increment(index, Num(1)),
        Multiply(index, Num(4)),
        Increment(index, arr),
        MemWrite(MemLoc(index, Num(0)), encode(newVal)),
        Assignment(destination, Num(1))
      )
    }

    case NewTuple(vs) => {
      val arr = Assignment(eax, Allocate(encode(Num(vs.size)),Num(1)))
      val sets = vs.zipWithIndex.map{ case (v,i) => MemWrite(MemLoc(eax, Num((i+1)*4)), encode(v)) }
      arr :: sets ::: List(Assignment(destination, eax))
    }

    case MakeClosure(l:Label, v:V) => compileD(NewTuple(List(l, v)), destination)
    case ClosureProc(v:V) => compileD(ARef(v, Num(0)), destination)
    case ClosureVars(v:V) => compileD(ARef(v, Num(1)), destination)
    case v:V => List(Assignment(destination, encode(v)))
  }

  private val count = Iterator.from(0)
  def temp() = Variable("__temp" + count.next())
  def tempLabel() = Label("__temp" + count.next())
}

trait L3ToL2Implicits {
  implicit def convertVar(v:Variable): L2Variable = L2Variable(v.name)
  implicit def convertNum(n:Num): L2Num = L2Num(n.n)
  implicit def convertLabel(l:Label): L2Label = L2Label(l.name)
  implicit def convertVToS(v:V): S = v match {
    case Num(n) => L2Num(n)
    case v:Variable => convertVar(v)
    case Label(name) => L2Label(name)
  }
  def declare(l:Label) = LabelDeclaration(L2Label(l.name))
}
