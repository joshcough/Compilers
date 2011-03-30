package L1Compiler.X86

import L1Compiler.L1AST.{Instruction => L1Instruction, _}

object JVMInst {
  type JVMInst = String
  def apply(is: JVMInst*) = List(is: _*)
  def dump(insts: List[JVMInst]) = insts.map { i =>
    (if (i.endsWith(":") || i.startsWith(".globl")) i else "\t" + i) + "\n"
  }.mkString
}

trait JavaByteCodeGenerator extends L1Compiler.BackEnd {

  import JVMInst._

  def generateCode(ast: L1): String = {
    val header: List[JVMInst] =
      JVMInst(
        ".class public Foo",
        ".super java/lang/Object")
//        ".file	\"prog.c\"",
//        ".text",
//        ".globl go",
//        ".type	go, @function",
//        "go:",
//        "pushl %ebx",
//        "pushl %esi",
//        "pushl %edi",
//        "pushl %ebp",
//        "movl	%esp, %ebp")
    def footer =
      JVMInst(
	      ".size	go, .-go",
	      ".ident	\"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2\"",
	      ".section	.note.GNU-stack,\"\",@progbits")
    JVMInst.dump(header) +
            JVMInst.dump(generateMain(ast.main) ::: ast.funs.flatMap(generateFunc)) +
            JVMInst.dump(footer)
  }

  private def generateMain(main: Func):List[JVMInst] = {
    val footer:List[JVMInst] =
      JVMInst(
        "popl %ebp",
        "popl %edi",
        "popl %esi",
        "popl %ebx",
        "leave",
        "ret")
    main.body.flatMap(genInst) ::: footer
  }

  private def generateFunc(f: Func):List[JVMInst] = {
    genInst(f.name) ::: f.body.flatMap(genInst)
  }

  def genInst(inst: L1Instruction): List[JVMInst] = {

    def jump(x:X) = x match {
      case Label(name) => "jmp L1_" + name
      case _ => "jmp *" + genInst(x).head
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

    def tri(theOp:String, s1:L1Instruction, s2:L1Instruction): String = triple(theOp,genInst(s1).head,s2)
    def triple(theOp:String, s1:String, s2:L1Instruction): String = {
      theOp + " " + s1 + ", " + genInst(s2).head
    }

    inst match {
      case Num(n) => JVMInst("$" + n)
      case Label(l) => JVMInst("$L1_" + l)
      case LabelDeclaration(l) => JVMInst(declare(l))
      case r:Register => JVMInst("%" + r.name)
      case MemLoc(r, off) => JVMInst(off.n + "(" + genInst(r).head + ")")

      case Assignment(cx:CXRegister, c@Comp(r:Register,op,x:X)) => {
        JVMInst(
          tri("cmp", x, r),
          setInstruction(op) + " " + cx.low8,
          triple("movzbl", cx.low8, cx))
      }
      case Assignment(cx:CXRegister, c@Comp(n1:Num,op,n2:Num)) =>
        JVMInst(triple("movl", "$" + (if(op(n1.n, n2.n)) 1 else 0), cx))
      case Assignment(r1, s) => JVMInst(tri("movl", s, r1))

      case MemWrite(loc, s) => JVMInst(tri("movl", s, loc))
      case MemRead(loc) => genInst(loc)
      case Increment(r, s) => JVMInst(tri("addl", s, r))
      case Decrement(r, s) => JVMInst(tri("subl", s, r))
      case Multiply(r, s) => JVMInst(tri("imull", s, r))
      case RightShift(r, s) => JVMInst(tri("sarl", s, r))
      case LeftShift(r, s) => JVMInst(tri("sall", s, r))
      case BitwiseAnd(r, s) => JVMInst(tri("andl", s, r))
      case Comp(s1:X, _, s2:X) => JVMInst(tri("cmpl", s2, s1))

      case Print(s) =>
        JVMInst(
          "pushl " + genInst(s).head,
          "call print",
          "addl $4, %esp")

      case Allocate(s, n) =>
        JVMInst(
          "pushl " + genInst(n).head,
          "pushl " + genInst(s).head,
          "call allocate", "addl $8, %esp")

      case Goto(s) => JVMInst(jump(s))

      case Call(s) => {
        val label = nextNewLabel
        val jmp = s match { case Label(name) => name; case _ => genInst(s) }
        JVMInst(
          "pushl " + genInst(label).head,
          "pushl %ebp",
          "movl %esp, %ebp",
          jump(s),
          declare(label))
      }

      /////////// cjump //////////

      // special case for two numbers
      case CJump(Comp(n1:Num, op, n2:Num), l1, l2) => {
        if(op(n1.n, n2.n)) JVMInst(jump(l1)) else JVMInst(jump(l2))
      }

      // (cjump 11 < ebx :true :false) // special case. destination just be a register.
      case CJump(Comp(n:Num, op, r:Register), l1, l2) => {
        val jumpInstruction = op match {
          case LessThan => jumpIfGreater(l1)
          case LessThanOrEqualTo => jumpIfGreaterOrEqual(l1)
          case EqualTo => jumpIfEqual(l1)
        }
        JVMInst(
          // magic reversal happens here.
          // LessThan ignored. had to pick one. see genCode(Comp...)
          genInst(Comp(r,LessThan,n)).head,
          jumpInstruction,
          jump(l2))
      }

      case CJump(cmp@Comp(s1,op,s2), l1, l2) => {
        val jumpInstruction = op match {
          case LessThan => jumpIfLess(l1)
          case LessThanOrEqualTo => jumpIfLessThanOrEqual(l1)
          case EqualTo => jumpIfEqual(l1)
        }
        JVMInst(
          genInst(cmp).head,
          jumpInstruction,
          jump(l2))
      }

      case Return =>
        JVMInst(
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
