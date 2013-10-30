package L2Compiler

object L2Printer extends L2Printer

trait L2Printer {
  import L2AST._
  def toCode(a:L2ASTNode): String = a match {
    case L2(main, funcs) => (main :: funcs).map(toCode).mkString("(", "\n", ")")
    case f:Func => f.body.map(toCode).mkString("(", "\n", ")")
    case Allocate(n:S, init:S) => "(allocate " + toCode(n) + " " + toCode(init) + ")"
    case Assignment(x:X, rhs:AssignmentRHS) => "(" + toCode(x) + " <- " + toCode(rhs) + ")"
    case Increment(x:X, s:S)  => "(" + toCode(x) + " += " +  toCode(s) + ")"
    case Decrement(x:X, s:S)  => "(" + toCode(x) + " -= " +  toCode(s) + ")"
    case Multiply(x:X, s:S)   => "(" + toCode(x) + " *= " +  toCode(s) + ")"
    case BitwiseAnd(x:X, s:S) => "(" + toCode(x) + " &= " +  toCode(s) + ")"
    case LeftShift(x:X, s:S)  => "(" + toCode(x) + " <<= " + toCode(s) + ")"
    case RightShift(x:X, s:S) => "(" + toCode(x) + " >>= " + toCode(s) + ")"
    case MemLoc(basePointer:X, offset: Num) => "(mem " + toCode(basePointer) + " " + toCode(offset) + ")"
    case MemRead(loc: MemLoc) => toCode(loc)
    case MemWrite(loc: MemLoc, s:S) => "(" + toCode(loc) + " <- " + toCode(s) + ")"
    case Print(s:S) => "(print " + toCode(s) + ")"
    case Goto(s:S) => "(goto " + toCode(s) + ")"
    case CJump(comp:Comp, l1: Label, l2: Label) => "(cjump " + toCode(comp) + " " + toCode(l1)  + " " + toCode(l2) + ")"
    case Call(s:S) => "(call " + toCode(s) + ")"
    case TailCall(s:S) => "(tail-call " + toCode(s) + ")"
    case Return => "(return)"
    case ArrayError(s1:S, s2:S) => "(array-error " + toCode(s1) + " " + toCode(s2) + ")"
    case Comp(s1:S, op: CompOp, s2:S) => toCode(s1) + " " + op.op + " " + toCode(s2)
    case Num(n) => n.toString
    case Label(name: String) => ":" + name
    case LabelDeclaration(l: Label) => toCode(l)
    case r:Register => r.name
    case v:Variable => v.name
  }

  def printAllocation(opairs: Option[Map[Variable, Register]]) = opairs match {
    case Some(pairs) =>
      pairs.keys.toList.sortWith(_<_).map(v =>
        "(" + toCode(v) + " " + toCode(pairs(v)) + ")").mkString("(", " ", ")")
    case _ => "#f"
  }

  def toCodeSorted(is:Iterable[L2ASTNode]): String = is.map(toCode).toList.sorted.mkString("(", " ", ")")

  def toCode(iios:InstructionInOutSet): String =
    "(" + toCode(iios.inst) + " " + toCodeSorted(iios.in) + " " + toCodeSorted(iios.out) + ")"

  def hwView(inouts: List[InstructionInOutSet]) = {
    val inSet = inouts.map(_.in).map(L2Printer.toCodeSorted).mkString(" ")
    val outSet = inouts.map(_.out).map(toCodeSorted).mkString(" ")
    "((in " + inSet + ") (out " + outSet + "))"
  }
  def testView(inouts: List[InstructionInOutSet]) = inouts.map(toCode).mkString("\n")

  def toCode(lr:LiveRange): String = "(" + toCode(lr.x) + " " + lr.range + ")"
  def printLiveRanges(ranges:List[List[LiveRange]]) =
    ranges.map(_.map(toCode).mkString(" ")).mkString("(", "\n", ")")
}