package L2Compiler

import L2AST._

trait L2Parser {

  def parse(exp:Any): L2 = exp match {
    case (main:List[_]) :: funcs => L2(parseMain(main), funcs map parseFunction)
    case _ => error("bad L2 program: " + exp)
  }

  def parseMain(exp: List[Any]) = Func(LabelDeclaration(Label("main")), exp map parseInstruction)
  def parseFunction(exp: Any): Func = exp match {
    case (l:Symbol) :: xs => Func(LabelDeclaration(parseLabel(l.toString)), xs map parseInstruction)
    case _ => error("bad function: " + exp)
  }

  def parseInstructionList(xs: List[Any]) = xs map parseInstruction

  def parseInstruction(expr: Any): Instruction = expr match {
    case s: Symbol => parseS(s) match {
      case l:Label => LabelDeclaration(l)
      // a register by itself is not an instruction. it lives inside of instructions.
      case r => error("not an instuction: " + r)
    }
    // a number by itself is not an instruction. it lives inside of instructions.
    case n: Int => error("not an instuction: " + n)
    case List(x: Any, '<-, s: Any) => parseAssignment(x, s)
    case List(x: Any, '<-, s1: Any, cmp: Symbol, s2:Any) => parseAssignment(x, (s1, cmp, s2))
    // math ops
    case List(s1: Symbol, op:Symbol, s2:Any) => op match {
      case '+= => Increment(parseX(s1), parseNumOrRegisterOrVar(s2))
      case '-= => Decrement(parseX(s1), parseNumOrRegisterOrVar(s2))
      case '*= => Multiply(parseX(s1), parseNumOrRegisterOrVar(s2))
      case '&= => BitwiseAnd(parseX(s1), parseNumOrRegisterOrVar(s2))
      // TODO (ebx <<= ecx) right side must be ecx
      case '>>= => RightShift(parseX(s1), parseNumOrVarOrEcx(s2))
      case '<<= => LeftShift(parseX(s1), parseNumOrVarOrEcx(s2))
    }
    case List('goto, s:Symbol) => Goto(parseLabel(s.toString))
    case List('call, s:Symbol) => Call(parseLabelOrRegisterOrVar(s))
    case List(Symbol("tail-call"), s:Symbol) => TailCall(parseLabelOrRegisterOrVar(s))
    case List('return) => Return
    // (cjump s cmp s label label) ;; conditional jump
    case 'cjump :: _ => parseCJump(expr)
    case _ => error("unexpected token: " + expr)
  }

  def parseS(exp:Any) = exp match {
    // num   ::= number between (inclusive) -2^31 and (2^31)-1
    // TODO: check range
    case n: Int => Num(n)
    case s: Symbol => parseLabelOrRegisterOrVar(s)
  }

  def parseNumOrRegisterOrVar(exp:Any): S = exp match {
    case n: Int => Num(n)
    case s: Symbol => parseX(s)
  }

  def parseNumOrVarOrEcx(exp:Any): S = parseNumOrRegisterOrVar(exp) match {
    case n:Num => n
    case v:Variable => v
    case e if e == ecx => e
    case _ => error("expected number, variable, or ecx. got: " + exp)
  }

  def parseLabelOrRegisterOrVar(s: Symbol): S = {
    if (s.toString.startsWith("':")) parseLabel(s.toString) else parseX(s)
  }

  // label ::= sequence of alpha-numeric characters or underscore,
  // but starting with a colon, ie matching this regexp:
  // #rx"^:[a-zA-Z0-9_]$"
  def parseLabel(s: String) = {
    val chop = s.toString.drop(1) // remove the ' from ':label
    // TODO: put some legit error checking here...
    Label(s.drop(2)) // remove the ' and : from ':label.
  }

  def parseX(s: Symbol): X = {
    XRegister(s).getOrElse(CXRegister(s).getOrElse(Variable(s.toString.drop(1))))
  }

  def parseCxRegisterOrVar(s: Symbol): X = CXRegister(s).getOrElse(Variable(s.toString.drop(1)))

  // TODO: isnt there an ecx only case in here somewhere? look at L1Parser
  def parseComp(s1: Any, cmp: Symbol, s2: Any): Comp = {
    Comp(parseNumOrRegisterOrVar(s1), parseCompOp(cmp),parseNumOrRegisterOrVar(s2))
  }

  def parseCompOp(s:Symbol): CompOp = s match {
    case '< => LessThan
    case '<= => LessThanOrEqualTo
    case '= => EqualTo
    case _ => error(s + " is an invalid comparison operator")
  }

  def parseCJump(exp: Any) = exp match {
    case List('cjump, s1, cmp: Symbol, s2, l1: Symbol, l2: Symbol) =>
      CJump(parseComp(s1, cmp, s2), parseLabel(l1.toString), parseLabel(l2.toString))
    case _ => error("bad cjump: " + exp)
  }

  def parseAssignment(x: Any, s: Any): Instruction = {
    (x, '<-, s) match {
      //(x <- s) ;; assign to a register or variable
      case (x: Symbol, '<-, i:Int) => Assignment(parseX(x), Num(i))
      case (x: Symbol, '<-, s:Symbol) => Assignment(parseX(x), parseLabelOrRegisterOrVar(s))
      // | (x <- (mem x n4))   ;; read from memory @ x+n4
      case (x1: Symbol, '<-, List('mem, x2:Symbol, n4:Int)) =>
        Assignment(parseX(x1), MemRead(MemLoc(parseX(x2), Num(n4))))
      // | ((mem x n4) <- s)   ;; update memory @ x+n4
      case (List('mem, x:Symbol, n4:Int), '<-, x2) =>
        MemWrite(MemLoc(parseX(x), Num(n4)), parseS(x2))
      // ;; two calls into the runtime system, one to print a value:
      //(eax <- (print s))
      // TODO: figure out if this is even legal (print 8)
      case ('eax, '<-, List('print, s)) => Assignment(eax, Print(parseNumOrRegisterOrVar(s)))
      // ;; and one to allocate & initialize some space
      // (eax <- (allocate s s))
      case ('eax, '<-, List('allocate, s1, s2)) =>
        Assignment(eax, Allocate(parseNumOrRegisterOrVar(s1), parseNumOrRegisterOrVar(s2)))
      case ('eax, '<-, List(Symbol("array-error"), s1, s2)) =>
        Assignment(eax, ArrayError(parseNumOrRegisterOrVar(s1), parseNumOrRegisterOrVar(s2)))
      //(cx <- s cmp s)
      case (cx:Symbol, '<-, (s1: Any, cmp: Symbol, s2: Any)) =>
        Assignment(parseCxRegisterOrVar(cx), parseComp(s1, cmp, s2))
      case _ => error("bad assignment statement")
    }
  }
}