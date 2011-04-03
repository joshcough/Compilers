package L1Compiler.X86

import L1Compiler.L1AST.{Instruction => L1Instruction, _}

object X86Inst {
  type X86Inst = String
  def apply(is: X86Inst*) = List(is: _*)
  def dump(insts: List[X86Inst]) = {
    insts.map {
      i =>
        (if (i.endsWith(":") || i.startsWith(".globl")) i else "\t" + i) + "\n"
    }.mkString
  }
}

trait X86Generator extends L1Compiler.BackEnd {

  import X86Inst._

  def generateCode(ast: L1, originalFileName:String): String = {
    val header: List[X86Inst] =
      X86Inst(
        ".file	\"prog.c\"",
        ".text",
        ".globl go",
        ".type	go, @function",
        "go:",
        "pushl %ebx",
        "pushl %esi",
        "pushl %edi",
        "pushl %ebp",
        "movl	%esp, %ebp")
    def footer =
      X86Inst(
	      ".size	go, .-go",
	      ".ident	\"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2\"",
	      ".section	.note.GNU-stack,\"\",@progbits")
    X86Inst.dump(header) +
            X86Inst.dump(generateMain(ast.main) ::: ast.funs.flatMap(generateFunc)) +
            X86Inst.dump(footer)
  }

  private def generateMain(main: Func):List[X86Inst] = {
    val footer:List[X86Inst] =
      X86Inst(
        "popl %ebp",
        "popl %edi",
        "popl %esi",
        "popl %ebx",
        "leave",
        "ret")
    main.body.flatMap(genInst) ::: footer
  }

  private def generateFunc(f: Func):List[X86Inst] = {
    genInst(f.name) ::: f.body.flatMap(genInst)
  }

  def genInst(inst: L1Instruction): List[X86Inst] = {

    def jump(s:S) = s match {
      case Label(name) => "jmp L1_" + name
      case _ => "jmp *" + genInst(s).head
    }

    def jumpIfLess(l: Label) = "jl L1_" + l.l
    def jumpIfLessThanOrEqual(l: Label) = "jle L1_" + l.l
    def jumpIfGreater(l: Label) = "jg L1_" + l.l
    def jumpIfGreaterOrEqual(l: Label) = "jge L1_" + l.l
    def jumpIfEqual(l: Label) = "je L1_" + l.l

    def declare(l: Label) = "L1_" + l.l + ":"

    def setInstruction(op: CompOp) = op match {
      case LessThan => "setl"
      case LessThanOrEqualTo => "setle"
      case EqualTo => "sete"
    }

    def tri(theOp:String, s1:L1Instruction, s2:L1Instruction) = triple(theOp,genInst(s1).head,s2)
    def triple(theOp:String, s1:String, s2:L1Instruction): String = {
      theOp + " " + s1 + ", " + genInst(s2).head
    }

    inst match {
      case Num(n) => X86Inst("$" + n)
      case Label(l) => X86Inst("$L1_" + l)
      case LabelDeclaration(l) => X86Inst(declare(l))
      case r:Register => X86Inst("%" + r.name)
      case MemLoc(r, off) => X86Inst(off.n + "(" + genInst(r).head + ")")

      // several assignment cases.
      case Assignment(r:Register, s:S) => X86Inst(tri("movl", s, r))
      case Assignment(r:Register, MemRead(loc)) => X86Inst(tri("movl", loc, r))
      // cmp assignments have to be with CXRegisters on LHS
      /**
          (eax <- ebx < ecx)
          Here we need another trick; the x86 instruction set only let us
          update the lowest 8 bits with the result of a condition code. So,
          we do that, and then fill out the rest of the bits with zeros with
          a separate instruction:

          cmp %ecx, %ebx
          setl %al
          movzbl %al, %eax
       */
      // TODO: these 3 cases are the same basically...see if they can be cleaned up
      case Assignment(cx:CXRegister, c@Comp(left:Register,op,right:Register)) => {
        X86Inst(
          tri("cmp", right, left),
          setInstruction(op) + " " + cx.low8,
          triple("movzbl", cx.low8, cx))
      }
      case Assignment(cx:CXRegister, c@Comp(left:Num,op,right:Register)) => {
        X86Inst(
          tri("cmp", right, left),
          setInstruction(op) + " " + cx.low8,
          triple("movzbl", cx.low8, cx))
      }
      case Assignment(cx:CXRegister, c@Comp(left:Register,op,right:Num)) => {
        X86Inst(
          tri("cmp", right, left),
          setInstruction(op) + " " + cx.low8,
          triple("movzbl", cx.low8, cx))
      }
      case Assignment(cx:CXRegister, c@Comp(n1:Num,op,n2:Num)) =>
        X86Inst(triple("movl", "$" + (if(op(n1.n, n2.n)) 1 else 0), cx))

      // cx must be eax here.
      case Assignment(cx:CXRegister, Print(s)) =>
        X86Inst(
          "pushl " + genInst(s).head,
          "call print",
          "addl $4, %esp")
      // cx must be eax here.
      case Assignment(cx:CXRegister, Allocate(s, n)) =>
        X86Inst(
          "pushl " + genInst(n).head,
          "pushl " + genInst(s).head,
          "call allocate", "addl $8, %esp")

      case Assignment(l, r) => error("bad assignment statement: " + inst)


      case MemWrite(loc, s) => X86Inst(tri("movl", s, loc))
      case Increment(r, s) => X86Inst(tri("addl", s, r))
      case Decrement(r, s) => X86Inst(tri("subl", s, r))
      case Multiply(r, s) => X86Inst(tri("imull", s, r))
      case RightShift(r, s) => X86Inst(tri("sarl", s, r))
      case LeftShift(r, s) => X86Inst(tri("sall", s, r))
      case BitwiseAnd(r, s) => X86Inst(tri("andl", s, r))

      case Goto(s) => X86Inst(jump(s))

      case Call(s) => {
        val label = nextNewLabel
        val jmp = s match { case Label(name) => name; case _ => genInst(s) }
        X86Inst(
          "pushl " + genInst(label).head,
          "pushl %ebp",
          "movl %esp, %ebp",
          jump(s),
          declare(label))
      }

      /////////// cjump //////////

      // special case for two numbers
      case CJump(Comp(n1:Num, op, n2:Num), l1, l2) => {
        if(op(n1.n, n2.n)) X86Inst(jump(l1)) else X86Inst(jump(l2))
      }

      // (cjump 11 < ebx :true :false) // special case. destination jmust be a register.
      case CJump(Comp(n:Num, op, r:Register), l1, l2) => {
        val jumpInstruction = op match {
          case LessThan => jumpIfGreater(l1)
          case LessThanOrEqualTo => jumpIfGreaterOrEqual(l1)
          case EqualTo => jumpIfEqual(l1)
        }
        X86Inst(
          // magic reversal happens here.
          tri("cmpl", n, r),
          jumpInstruction,
          jump(l2))
      }

      case CJump(cmp@Comp(s1,op,s2), l1, l2) => {
        val jumpInstruction = op match {
          case LessThan => jumpIfLess(l1)
          case LessThanOrEqualTo => jumpIfLessThanOrEqual(l1)
          case EqualTo => jumpIfEqual(l1)
        }
        X86Inst(
          tri("cmpl", s2, s1),
          jumpInstruction,
          jump(l2))
      }

      case Return =>
        X86Inst(
          "movl %ebp, %esp",
          "popl %ebp",
          "ret")
    }
  }

  // figure out a better way to do this crap:
  private var labelCount = -1
  private def nextNewLabel = {
    labelCount+=1
    Label("Generated_Label_" + labelCount)
  }
}
