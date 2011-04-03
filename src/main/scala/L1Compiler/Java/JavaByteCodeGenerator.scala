package L1Compiler.Java

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

  def generateCode(ast: L1, originalFileName:String): String = {
    val className = originalFileName.reverse.takeWhile(_!='/').dropWhile(_!='.').drop(1).reverse
    val header: List[JVMInst] =
      JVMInst(
        ".class public " + className,
        ".super java/lang/Object",
        "; constructor",
        ".method public <init>()V",
        "  aload_0",
        "  invokenonvirtual java/lang/Object/<init>()V",
        "  return",
        ".end method",
        ".method public static main([Ljava/lang/String;)V",
        "  .limit stack 99",
        "  .limit locals 99")
    def footer =
      JVMInst(
        "  return",
        ".end method",
        "; class init",
        ".method public <clinit>()V",
        "  .limit stack 99",
        "  .limit locals 99",
        "  return",
        ".end method")
    JVMInst.dump(header) +
            JVMInst.dump(generateMain(ast.main) ::: ast.funs.flatMap(generateFunc)) +
            JVMInst.dump(footer)
  }

  private def generateMain(main: Func):List[JVMInst] = {
    main.body.flatMap(genInst).map("  " + _)
  }

  private def generateFunc(f: Func):List[JVMInst] = {
    (genInst(f.name) ::: f.body.flatMap(genInst)).map("  " + _)
  }

  def genInst(inst: L1Instruction): List[JVMInst] = {

     inst match {

      // several assignment cases.
      case Assignment(r:Register, s:S) =>
        JVMInst(
          loadRegisterOntoStack(r),
          loadValueOntoStackAsObject(s),
          invokeMov)

      case Assignment(r:Register, MemRead(MemLoc(base, off))) => {
        JVMInst(
          loadRegisterOntoStack(r),
          loadRegisterOntoStack(base),
          loadValueOntoStackAsInt(off),
          invokeRead,
          invokeMov
        )
      }

      case Assignment(cx:CXRegister, c@Comp(left:S,op,right:S)) => {
        val trueLabel = nextNewLabel()
        val finishLabel = nextNewLabel()
        JVMInst(
          ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
          ";;; assignment to comparison ;;;",
          ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",
          "; load the register to store 0 or 1",
          loadRegisterOntoStack(cx),
          "; load ints to compare",
          loadValueOntoStackAsInt(left),
          loadValueOntoStackAsInt(right),
          "; compare them",
          comparisonInstruction(op) + " L1_" + trueLabel.name,
          "; put 0 on stack",
          loadValueOntoStackAsObject(Num(0)),
          "goto L1_" + finishLabel.name,
          declare(trueLabel),
          "; put 1 on stack",
          loadValueOntoStackAsObject(Num(1)),
          declare(finishLabel),
          "; store",
          invokeMov,
          ";;;;end assignment to comparison;;;;")
      }

      // cx has to be eax here or it wouldnt get through parsing
      case Assignment(cx:CXRegister, Print(s)) => {
        JVMInst(
          loadValueOntoStackAsObject(s),
          "invokestatic L1Compiler/Java/L1JavaRuntime/print(Ljava/lang/Object;)V")
      }
      // cx has to be eax here or it wouldnt get through parsing
      case Assignment(cx:CXRegister, Allocate(s, n)) => {
        JVMInst(
          // TODO: put the result of the last instruction here into eax.
          // or maybe the allocate function can do it automatically. whatever.
          loadValueOntoStackAsInt(s),
          loadValueOntoStackAsInt(n),
          "invokestatic L1Compiler/Java/L1JavaRuntime/allocate(II)I")
      }

      case Assignment(l, r) => error("bad assignment statement: " + inst)

      case LabelDeclaration(l:Label) => JVMInst(declare(l))

//      case MemWrite(loc, s) => JVMInst(tri("movl", s, loc))
//      case Increment(r, s) => JVMInst(tri("addl", s, r))
//      case Decrement(r, s) => JVMInst(tri("subl", s, r))
//      case Multiply(r, s) => JVMInst(tri("imull", s, r))
//      case RightShift(r, s) => JVMInst(tri("sarl", s, r))
//      case LeftShift(r, s) => JVMInst(tri("sall", s, r))
//      case BitwiseAnd(r, s) => JVMInst(tri("andl", s, r))
//
//      case Goto(s) => JVMInst(jump(s))
//
//      case Call(s) => {
//        val label = nextNewLabel
//        val jmp = s match { case Label(name) => name; case _ => genInst(s) }
//        JVMInst(
//          "pushl " + genInst(label).head,
//          "pushl %ebp",
//          "movl %esp, %ebp",
//          jump(s),
//          declare(label))
//      }
//
//      /////////// cjump //////////
//
//      // special case for two numbers
//      case CJump(Comp(n1:Num, op, n2:Num), l1, l2) => {
//        if(op(n1.n, n2.n)) JVMInst(jump(l1)) else JVMInst(jump(l2))
//      }
//
//      // (cjump 11 < ebx :true :false) // special case. destination jmust be a register.
//      case CJump(Comp(n:Num, op, r:Register), l1, l2) => {
//        val jumpInstruction = op match {
//          case LessThan => jumpIfGreater(l1)
//          case LessThanOrEqualTo => jumpIfGreaterOrEqual(l1)
//          case EqualTo => jumpIfEqual(l1)
//        }
//        JVMInst(
//          // magic reversal happens here.
//          tri("cmpl", n, r),
//          jumpInstruction,
//          jump(l2))
//      }
//
//      case CJump(cmp@Comp(s1,op,s2), l1, l2) => {
//        val jumpInstruction = op match {
//          case LessThan => jumpIfLess(l1)
//          case LessThanOrEqualTo => jumpIfLessThanOrEqual(l1)
//          case EqualTo => jumpIfEqual(l1)
//        }
//        JVMInst(
//          tri("cmpl", s2, s1),
//          jumpInstruction,
//          jump(l2))
//      }
//
//      case Return =>
//        JVMInst(
//          "movl %ebp, %esp",
//          "popl %ebp",
//          "ret")

      case _ => error("implement me: " + inst)
    }
  }

  def loadValueOntoStackAsInt(s: S) = s match {
    case Num(n) => "ldc " + n
    case Label(name) => error("what do i do here?")
    case r:Register =>
      loadRegisterOntoStack(r) + "\n" +
      "invokevirtual L1Compiler/Java/JavaRuntimeRegister/getIntValue()I"
  }

  def loadValueOntoStackAsObject(s: S) = s match {
    case Num(n) => "ldc " + n + "\n" +
      "invokestatic scala/runtime/BoxesRunTime/boxToInteger(I)Ljava/lang/Integer;"
    case Label(name) => error("what do i do here?")
    case r:Register =>
      loadRegisterOntoStack(r) + "\n" +
      "invokevirtual L1Compiler/Java/JavaRuntimeRegister/value()Ljava/lang/Object;"
  }

  // puts the register directly on the stack. not its value.
  def loadRegisterOntoStack(r: Register) =
    "invokestatic L1Compiler/Java/L1JavaRuntime/" + r.name + "()LL1Compiler/Java/JavaRuntimeRegister;"

  def declare(l: Label) = "L1_" + l.name + ":"

  def invokeMov =
    "invokestatic L1Compiler/Java/L1JavaRuntime/mov(LL1Compiler/Java/JavaRuntimeRegister;Ljava/lang/Object;)V"

  def invokeRead =
    "invokestatic L1Compiler/Java/L1JavaRuntime/read(LL1Compiler/Java/JavaRuntimeRegister;I)Ljava/lang/Object;"

  def comparisonInstruction(op: CompOp) = op match {
    case LessThan => "if_icmplt"
    case LessThanOrEqualTo => "if_icmple"
    case EqualTo => "if_icmpeq"
  }

  private val labels = Iterator.from(0)
  private def nextNewLabel() = Label("Generated_Label_" + labels.next())
}
