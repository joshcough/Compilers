package L1Compiler.Java

import L1Compiler.L1AST.{Instruction => L1Instruction, _}

object JVMInst {
  type JVMInst = String
  def apply(is: JVMInst*) = List(is: _*)
  def dump(insts: List[JVMInst]) = insts.map { i =>
    (if (i.endsWith(":") || i.startsWith(".globl")) i else "\t" + i) + "\n"
  }.mkString
}

/**
 *
 Example output for Jasmin:

.class public goo
.super java/lang/Object

; constructor
.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
  .limit stack 99
  .limit locals 99
  getstatic java/lang/System/out Ljava/io/PrintStream;
  ldc "hi"
  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  return
.end method

; class init
.method public <clinit>()V
  .limit stack 99
  .limit locals 99
  return
.end method
 */
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

//  def gen(s:S): List[JVMInst] = {
//  }
  
  def genInst(inst: L1Instruction): List[JVMInst] = {

//    def jump(s:S) = s match {
//      case Label(name) => "jmp L1_" + name
//      case _ => "jmp *" + genInst(s).head
//    }
//
//    def jumpIfLess(l: Label) = "jl L1_" + l.l
//    def jumpIfLessThanOrEqual(l: Label) = "jle L1_" + l.l
//    def jumpIfGreater(l: Label) = "jg L1_" + l.l
//    def jumpIfGreaterOrEqual(l: Label) = "jge L1_" + l.l
//    def jumpIfEqual(l: Label) = "je L1_" + l.l
//
//    def declare(l: Label) = "L1_" + l.l + ":"
//
//    def setInstruction(op: CompOp) = op match {
//      case LessThan => "setl"
//      case LessThanOrEqualTo => "setle"
//      case EqualTo => "sete"
//    }
//
//    def tri(theOp:String, s1:L1Instruction, s2:L1Instruction) = triple(theOp,genInst(s1).head,s2)
//    def triple(theOp:String, s1:String, s2:L1Instruction): String = {
//      theOp + " " + s1 + ", " + genInst(s2).head
//    }

    inst match {
//      case Num(n) => JVMInst("$" + n)
//      case Label(l) => JVMInst("$L1_" + l)
//      case LabelDeclaration(l) => JVMInst(declare(l))
//      case r:Register => JVMInst("%" + r.name)
//      case MemLoc(r, off) => JVMInst(off.n + "(" + genInst(r).head + ")")

      // several assignment cases.
      case Assignment(r:Register, s:S) =>
        JVMInst(
          "invokestatic L1Compiler/Java/L1JavaRuntime/" + r.name + "()LL1Compiler/Java/JavaRuntimeRegister;\n" +
           loadOntoStackAsObject(s),
          "invokestatic L1Compiler/Java/L1JavaRuntime/mov(LL1Compiler/Java/JavaRuntimeRegister;Ljava/lang/Object;)V")

//      case Assignment(r:Register, MemRead(loc)) => JVMInst(tri("movl", loc, r))
//      // cmp assignments have to be with CXRegisters on LHS
//      /**
//          (eax <- ebx < ecx)
//          Here we need another trick; the x86 instruction set only let us
//          update the lowest 8 bits with the result of a condition code. So,
//          we do that, and then fill out the rest of the bits with zeros with
//          a separate instruction:
//
//          cmp %ecx, %ebx
//          setl %al
//          movzbl %al, %eax
//       */
//      // TODO: these 3 cases are the same basically...see if they can be cleaned up
//      case Assignment(cx:CXRegister, c@Comp(left:Register,op,right:Register)) => {
//        JVMInst(
//          tri("cmp", right, left),
//          setInstruction(op) + " " + cx.low8,
//          triple("movzbl", cx.low8, cx))
//      }
//      case Assignment(cx:CXRegister, c@Comp(left:Num,op,right:Register)) => {
//        JVMInst(
//          tri("cmp", right, left),
//          setInstruction(op) + " " + cx.low8,
//          triple("movzbl", cx.low8, cx))
//      }
//      case Assignment(cx:CXRegister, c@Comp(left:Register,op,right:Num)) => {
//        JVMInst(
//          tri("cmp", right, left),
//          setInstruction(op) + " " + cx.low8,
//          triple("movzbl", cx.low8, cx))
//      }
//      case Assignment(cx:CXRegister, c@Comp(n1:Num,op,n2:Num)) =>
//        JVMInst(triple("movl", "$" + (if(op(n1.n, n2.n)) 1 else 0), cx))

      // cx has to be eax here or it wouldnt get through parsing
      case Assignment(cx:CXRegister, Print(s)) => {
        JVMInst(
          loadOntoStackAsObject(s),
          "invokestatic L1Compiler/Java/L1JavaRuntime/print(Ljava/lang/Object;)V")
      }
      // cx has to be eax here or it wouldnt get through parsing
      case Assignment(cx:CXRegister, Allocate(s, n)) => {
        JVMInst(
          // TODO: put the result of the last instruction here into eax.
          // or maybe the allocate function can do it automatically. whatever.
          loadOntoStackAsInt(s),
          loadOntoStackAsInt(n),
          "invokestatic L1Compiler/Java/L1JavaRuntime/allocate(II)I")
      }

      case Assignment(l, r) => error("bad assignment statement: " + inst)
//
//
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

  // TODO: figure out a better way to do this crap:
  private var labelCount = -1
  private def nextNewLabel = {
    labelCount+=1
    Label("Generated_Label_" + labelCount)
  }

  def loadOntoStackAsInt(s: S) = s match {
    case Num(n) => "ldc " + n
    case Label(name) => error("what do i do here?")
    case r:Register =>
      "invokestatic L1Compiler/Java/L1JavaRuntime/" + r.name + "()LL1Compiler/Java/JavaRuntimeRegister;\n" +
      "invokevirtual L1Compiler/Java/JavaRuntimeRegister/getIntValue()I"
  }

  def loadOntoStackAsObject(s: S) = s match {
    case Num(n) => "ldc " + n + "\n" +
      "invokestatic scala/runtime/BoxesRunTime/boxToInteger(I)Ljava/lang/Integer;"
    case Label(name) => error("what do i do here?")
    case r:Register =>
      "invokestatic L1Compiler/Java/L1JavaRuntime/" + r.name + "()LL1Compiler/Java/JavaRuntimeRegister;\n" +
      "invokevirtual L1Compiler/Java/JavaRuntimeRegister/value()Ljava/lang/Object;"
  }
}
