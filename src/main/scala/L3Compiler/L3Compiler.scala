package L3Compiler

import L3AST._
import L2Compiler.L2AST.{
  eax => l2eax, ebx => l2ebx, ecx => l2ecx, edi => l2edi, edx => l2edx, esi => l2esi,
  Func => L2Func,
  LessThan => L2LessThan,
  LessThanOrEqualTo => L2LessThanOrEqualTo,
  EqualTo => L2EqualTo,
  Num => L2Num,
  Variable => L2Variable,
  X => L2X,
  Label => L2Label,
  Print => L2Print,
  Register => L2Register,
  Instruction => L2Instruction, _}
import L2Compiler.L2Printer

trait L3ToL2Implicits {
  val registerMappings: Map[Register, L2Register] =
    Map(eax->l2eax, ebx->l2ebx, ecx->l2ecx, edi->l2edi, edx->l2edx, esi->l2esi)

  implicit def convertReg(r:Register): L2Register = registerMappings(r)
  implicit def convertVar(v:Variable): L2Variable = L2Variable(v.name)
  implicit def convertNum(n:Num): L2Num = L2Num(n.n)
  implicit def convertLabel(l:Label): L2Label = L2Label(l.name)
  implicit def convertX(x:X): L2X = x match {
    case r:Register => convertReg(r)
    case v:Variable => convertVar(v)
  }
  implicit def convertVToS(v:V): S = v match {
    case Num(n) => L2Num(n)
    case x:X => convertX(x)
    case Label(name) => L2Label(name)
  }
  def declare(l:Label) = LabelDeclaration(L2Label(l.name))
}

class L3Compiler extends io.Reader with L3Parser with L3ToL2Implicits{

  def compileToString(code:String): String = L2Printer.toCode(compile(code))

  def compile(code:String): L2 = compile(parse(read(code)))

  def compile(p:L3): L2 = p match {
    // TODO: think i'll need to strip off the return at the end of main.
    case L3(main, funcs) => {
      val secret = Label("__secret__main__")
      val secretDec = LabelDeclaration(secret)
      val realMain = L2Func(LabelDeclaration(secret)::compileE(main))
      L2(main=L2Func(List(Call(secret))), funs = realMain :: (funcs map compileFunction))
    }
  }

  // (l (x ...) e)
  def compileFunction(f:Func): L2Func = {
    val argAssignments =
        for( (v, r) <- f.args.zip(List(ecx, edx, eax))) yield Assignment(v, r)
    L2Func((declare(f.label) :: argAssignments ::: compileE(f.body)) :+ Return)
  }

  def compileE(e:E): List[L2Instruction] = e match {
    // e ::= (let ([x d]) e) | (if v e e) | d
    case Let(x:X, d:D, body:E) => compileD(d=d, destination=x) ::: compileE(body)
    case IfStatement(v:V, t:E, f:E) => {
      val tmp = temp()
      val thenLabel = tempLabel()
      val elseLabel = tempLabel()
      List(
        compileD(v, tmp),
        List(CJump(Comp(tmp, L2EqualTo, Num(1)), elseLabel, thenLabel)),
        List(LabelDeclaration(elseLabel)),
        compileE(f),
        List(LabelDeclaration(thenLabel)),
        compileE(t)).flatten
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
    case x:X => convertX(x)
    case Label(name) => L2Label(name)
  }


  def compileFunCall(f:FunCall, destination:X, tailCall:Boolean): List[L2Instruction] = {
    val argAssignments =
      for( (r, v) <- List(ecx, edx, eax).zip(f.args map encode) ) yield Assignment(r, v)
    argAssignments ::: (
      if(tailCall) List(TailCall(f.v))
      else List(Call(f.v), Assignment(destination, eax))
    )
  }

  def compileD(d:D, destination: X): List[L2Instruction] = d match {
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
      Increment(destination, Num(1)),
      BitwiseAnd(destination, Num(1)),
      LeftShift(destination, Num(1)),
      Increment(destination, Num(1))
    )
      
    case NewArray(size:V, init:V) => List(
      Assignment(eax, Allocate(encode(size), encode(init))),
      Assignment(destination, eax)
    )

    // TODO: review assertions, particularly x vs variable
    // TODO...does this loc definitly have to be a num????
    //(x <- (mem s n4))
    case ARef(arr:V, loc:V) => {
      assert(arr.isInstanceOf[X])
      assert(loc.isInstanceOf[Num])
      List(
        Assignment(destination, MemRead(MemLoc(convertX(arr.asInstanceOf[X]), loc.asInstanceOf[Num] * 4))))
    }

    case ALen(arr:V) => List(
      Assignment(destination, MemRead(MemLoc(convertX(arr.asInstanceOf[X]), Num(0)))),
      LeftShift(destination, Num(1)),
      Increment(destination, Num(1))
    )

    // TODO: review assertions, particularly x vs variable
    //((mem x n4) <- s)
    //(let ([x (aset v1 v2 v3)]) ...)
    case ASet(arr:V, loc:V, newVal: V) => {
      assert(arr.isInstanceOf[X])
      assert(loc.isInstanceOf[Num])
      val tmp = temp()
      val boundsFailLabel = tempLabel()
      val boundsPassLabel = tempLabel()
      List(
        //`(,x <- ,(encode v2))
        Assignment(destination, encode(loc)),
        // `(,x >>= 1)
        RightShift(destination, Num(1)),
        // `(,tmp <- (mem v1))
        Assignment(tmp, MemRead(MemLoc(convertX(arr.asInstanceOf[X]), Num(0)))),
        //`(cjump ,x < ,tmp ,bounds-fail-label ,bounds-pass-label)
        CJump(Comp(tmp, L2LessThan, destination), boundsFailLabel, boundsPassLabel),
        //bounds-fail-label
        LabelDeclaration(boundsFailLabel),
        //`(eax <- (array-error v1 x))
        Assignment(eax, ArrayError(arr, destination)),
        //bounds-pass-label
        LabelDeclaration(boundsPassLabel),
        //`(,x *= 4)
        Multiply(destination, Num(4)),
        //`(,x += ,v1)
        Increment(destination, arr),
        //`((mem ,x 4) <- ,(encode v3))
        MemWrite(MemLoc(destination, Num(4)), encode(newVal)),
        //`(,x <- 1)   ;; put the final result for aset into x (always 0).
        Assignment(destination, Num(1))
      )
    }

    case NewTuple(vs) => {
      val setStatemets = vs.zipWithIndex.map{ case (v,i) => ASet(destination, Num(i), v) }
      val tmp = temp()
      compileD(NewArray(Num(vs.size), Num(0)), destination) ::: setStatemets.flatMap(compileD(_, tmp))
    }

    // TODO: make tests for these three
    case MakeClosure(l:Label, v:V) => compileD(NewTuple(List(l, v)), destination)
    case ClosureProc(v:V) => compileD(ARef(v, Num(0)), destination)
    case ClosureVars(v:V) => compileD(ARef(v, Num(1)), destination)

    case v:V => List(Assignment(destination, encode(v)))
  }

  private val count = Iterator.from(0)
  def temp() = Variable("__temp" + count.next())
  def tempLabel() = Label("__temp" + count.next())
}