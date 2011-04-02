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
        "  invokestatic L1Compiler/Java/L1JavaRuntime/printHeapView()V",
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

  // mov(eax, allocate(21, 5))


  private def generateMain(main: Func):List[JVMInst] = {
    main.body.flatMap(genInst).map("  " + _)
  }

  private def generateFunc(f: Func):List[JVMInst] = {
    (genInst(f.name) ::: f.body.flatMap(genInst)).map("  " + _)
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

      case Print(s) => {
        def getValue(x:X) = x match {
          case Num(n) => n
          case _ =>
            // TODO: have to make call to runtime to get the value of the register.
            // and then put it onto the stack
            // instead of ldc (load constant)
            error("implement me")
        }

        /**
  public void printInt()
    print(BoxesRunTime.boxToInteger(7));
  }
  public void printObj() {
    print(eax());
  }

  public void printInt();
    Code:
     0:   aload_0
     1:   ldc_w   #323; //int 7
     4:   invokestatic    #124; //Method scala/runtime/BoxesRunTime.boxToInteger:(I)Ljava/lang/Integer;
     7:   invokevirtual   #325; //Method print:(Ljava/lang/Object;)Ljava/lang/String;
     10:  pop
     11:  return

  public void printObj();
    Code:
     0:   aload_0
     1:   aload_0
     2:   invokevirtual   #328; //Method eax:()LL1Compiler/Java/L1JavaRuntime$Register;
     5:   invokevirtual   #325; //Method print:(Ljava/lang/Object;)Ljava/lang/String;
     8:   pop
     9:   return
         */
        JVMInst(
          "ldc " + getValue(s),
          "invokestatic scala/runtime/BoxesRunTime/boxToInteger(I)Ljava/lang/Integer;",
          "invokestatic L1Compiler/Java/L1JavaRuntime/print(Ljava/lang/Object;)V")
      }

      // mov(eax, allocate(21, 5))
      case Allocate(s, n) => {
        def getValue(x:X) = x match {
          case Num(n) => n
          case _ =>
            // TODO: have to make call to runtime to get the value of the register.
            // and then put it onto the stack
            // instead of ldc (load constant)
            error("implement me")
        }
        JVMInst(
          "ldc " + getValue(s),
          "ldc " + getValue(n),
          "invokestatic L1Compiler/Java/L1JavaRuntime/allocate(II)I")
      }

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
