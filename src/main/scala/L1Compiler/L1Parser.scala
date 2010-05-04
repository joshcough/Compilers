package L1Compiler

import reader._
import L1AST._

trait L1Parser extends Parser[L1] {

  def parse(exp:Any): L1 = exp match {
    case (main:List[_]) :: funcs => L1(parseMain(main), funcs map parseFunction)
    case _ => error("bad L1 program")
  }

  def parseMain(exp: List[Any]) = Func(LabelDeclaration(Label("main")), exp map parseInstruction)
  def parseFunction(exp: Any): Func = exp match {
    case (l:Symbol) :: xs => Func(LabelDeclaration(parseLabel(l.toString)), xs map parseInstruction)
    case _ => error("bad function: " + exp)
  }

  def parseInstruction(expr: Any): Instruction = expr match {
    // TODO: had to put this here because of a serious compiler bug. investigate.
    // (cjump s cmp s label label) ;; conditional jump
    case 'cjump :: xs => parseCJump(expr)
    case s: Symbol => parseLabelOrRegister(s) match {
      case l:Label => LabelDeclaration(l)
      case r => r
    }
    // num   ::= number between (inclusive) -2^31 and (2^31)-1
    // TODO: check range
    case n: Int => Num(n)
    case List(x: Any, '<-, s: Any) => parseAssignment(x, s)
    // TODO: maybe just call this directly instead of going through parseAssignment
    case List(x: Any, '<-, s1: Any, cmp: Symbol, s2:Any) => parseAssignment(x, (s1, cmp, s2))
    // math ops
    case List(s1: Symbol, op:Symbol, s2:Any) => op match {
      case '+= => Increment(parseRegister(s1), parseNumOrRegister(s2))
      case '-= => Decrement(parseRegister(s1), parseNumOrRegister(s2))
      case '*= => Multiply(parseRegister(s1), parseNumOrRegister(s2))
      case '>>= => RightShift(parseRegister(s1), parseNumOrRegister(s2))
      case '<<= => LeftShift(parseRegister(s1), parseNumOrRegister(s2))
      case '&= => BitwiseAnd(parseRegister(s1), parseNumOrRegister(s2))
    }

    case List('goto, s:Symbol) => Goto(parseLabelOrRegister(s))
    case List('call, s:Any) => Call(parseS(s))
    case List('return) => Return
    case _ => error("unexpected token: " + expr)
  }

  def parseS(exp:Any) = exp match {
    case n: Int => Num(n)
    case s: Symbol => parseLabelOrRegister(s)
  }

  def parseNumOrRegister(exp:Any): X = exp match {
    case n: Int => Num(n)
    case s: Symbol => parseRegister(s)
  }

  def parseLabelOrRegister(s: Symbol): X = {
    if (s.toString.startsWith("':")) parseLabel(s.toString) else parseRegister(s)
  }

  // label ::= sequence of alpha-numeric characters or underscore,
  // but starting with a colon, ie matching this regexp:
  // #rx"^:[a-zA-Z0-9_]$"
  def parseLabel(s: String) = {
    val chop = s.toString.drop(1) // remove the ' from ':label
    // TODO: put some legit error checking here...
    Label(s.drop(2)) // remove the ' and : from ':label.
  }

  def parseRegister(s: Symbol): Register = {
    XRegister(s).getOrElse(CXRegister(s).getOrElse(error(s.toString.drop(1) + " is an invalid register")))
  }

  def parseCxRegister(s: Symbol): CXRegister = {
    CXRegister(s).getOrElse(error(s.toString.drop(1) + " is an invalid cx register"))
  }

  def parseComp(s1: Any, cmp: Symbol, s2: Any): Comp = {
    Comp(parseNumOrRegister(s1), parseCompOp(cmp),parseNumOrRegister(s2))
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

  def parseAssignment(x: Any, s: Any) = {
    (x, '<-, s) match {
      //(x <- s) ;; assign to a register
      case (x: Symbol, '<-, i: Int) => Assignment(parseRegister(x), Num(i))
      case (x: Symbol, '<-, s: Symbol) => Assignment(parseRegister(x), parseLabelOrRegister(s))
      // | (x <- (mem x n4))   ;; read from memory @ x+n4
      case (x1: Symbol, '<-, List('mem, x2:Symbol, n4:Int)) =>
        Assignment(parseRegister(x1), MemRead(MemLoc(parseRegister(x2), Num(n4))))
      // | ((mem x n4) <- s)   ;; update memory @ x+n4
      case (List('mem, x:Symbol, n4:Int), '<-, x2) =>
        MemWrite(MemLoc(parseRegister(x), Num(n4)), parseNumOrRegister(x2))
      // ;; two calls into the runtime system, one to print a value:
      //(eax <- (print s))
      // TODO: figure out if this is even legal (print 8)
      case ('eax, '<-, List('print, s)) => Print(parseNumOrRegister(s))
      // ;; and one to allocate & initialize some space
      // (eax <- (allocate s s))
      case ('eax, '<-, List('allocate, s1, s2)) => Allocate(parseNumOrRegister(s1), parseNumOrRegister(s2))
      //(cx <- s cmp s)
      case (cx:Symbol, '<-, (s1: Any, cmp: Symbol, s2: Any)) =>
        Assignment(parseCxRegister(cx), parseComp(s1, cmp, s2))
      case _ => error("bad assignment statement")
    }
  }
}