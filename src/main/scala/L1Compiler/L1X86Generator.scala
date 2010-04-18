package L1Compiler

import L1AST.{Instruction => L1Instruction, _}

object L1X86Generator {

  type X86Inst = String
  object X86Inst{
    def apply(is:X86Inst*) = List(is:_*)
    def dump(insts: List[X86Inst]) = {
      insts.map{ i =>
        (if(i.endsWith(":") || i.startsWith(".globl")) i else "\t" + i) + "\n"
      }.mkString
    }
  }

  def generateCode(ast: L1): String = {
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
  
  def generateMain(main: L1Function):List[X86Inst] = {
    val footer:List[X86Inst] =
      X86Inst(
        "popl %ebp",
        "popl %edi",
        "popl %esi",
        "popl %ebx",
        "leave",
        "ret")
    main.body.flatMap(generateCode) ::: footer
  }

  def generateFunc(f: L1Function):List[X86Inst] = {
    generateCode(f.name) ::: f.body.flatMap(generateCode)
  }

  def generateCode(inst: L1Instruction): List[X86Inst] = {
    inst match {
      case Num(n) => X86Inst("$" + n)
      case Label(l) => X86Inst("$L1_" + l)
      case LabelDeclaration(l) => X86Inst(declare(l))
      case r:Register => X86Inst("%" + r.name)
      case MemLoc(r, off) => X86Inst(off.n + "(" + generateCode(r).head + ")")
      // TODO register assignment where s is a Comp
      case RegisterAssignment(r1, s) =>
        X86Inst("movl " + generateCode(s).head + ", " + generateCode(r1).head)
      case MemWrite(loc, s) =>
        X86Inst("movl " + generateCode(s).head + ", " + generateCode(loc).head)
      case MemRead(loc) => generateCode(loc)
      //edx += ecx  =>  addl %ecx, %edx
      case Increment(r, s) =>
        X86Inst("addl " + generateCode(s).head + ", " + generateCode(r).head)
      //edx += ecx  =>  addl %ecx, %edx
      case Decrement(r, s) =>
        X86Inst("subl " + generateCode(s).head + ", " + generateCode(r).head)
      // TODO: does this have to only work with eax somehow? 
      case Multiply(r, s) =>
        X86Inst("imull " + generateCode(s).head + ", " + generateCode(r).head)

      case Print(s) => X86Inst("pushl " + generateCode(s).head, "call print", "addl $4,%esp")
      case Allocate(s, n) =>
        X86Inst(
          "pushl " + generateCode(n).head,
          "pushl " + generateCode(s).head,
          "call allocate", "addl $8, %esp")
      case Goto(s) => X86Inst(jump(s))
      case Call(s) => {
        val label = nextNewLabel
        val jmp = s match { case Label(name) => name; case _ => generateCode(s) }
        X86Inst(
          "pushl " + generateCode(label).head,
          "pushl %ebp", "movl %esp, %ebp",
          jump(s),
          declare(label))
      }

      case Comp(s1:S, LessThan, s2:S) =>
        X86Inst("cmpl " + generateCode(s2).head + ", " + generateCode(s1).head) // check this!

      /////////// cjump < //////////

      // special case for two numbers
      case CJump(Comp(n1:Num, LessThan, n2:Num), l1, l2) =>
        if(n1.n < n2.n) X86Inst(jump(l1)) else X86Inst(jump(l2))
      case CJump(Comp(n1:Num, LessThanOrEqualTo, n2:Num), l1, l2) =>
        if(n1.n <= n2.n) X86Inst(jump(l1)) else X86Inst(jump(l2))
      // (cjump 11 < ebx :true :false) // special case. destination just be a register.
      case CJump(Comp(n:Num, LessThan, r:Register), l1, l2) =>
        X86Inst(
          "cmpl " + generateCode(n).head + ", " + generateCode(r),
          jumpIfGreater(l1),
          jump(l2))
      case CJump(Comp(s1,LessThan,s2), l1, l2) =>
        generateCode(Comp(s1,LessThan,s2)) ::: X86Inst(jumpIfLess(l1), jump(l2))

      /////////// cjump == //////////
      
      case CJump(Comp(r:Register, EqualTo, s:S), l1, l2) =>
        X86Inst(
          "cmpl " + generateCode(s).head + ", " + generateCode(r).head,
          jumpIfEqual(l1),
          jump(l2))

      case Return =>
        X86Inst(
          "movl %ebp, %esp",
          "popl %ebp",
          "ret")
    }
  }

  // figure out a better way to do this crap:
  var labelCount = -1
  def nextNewLabel = {
    labelCount+=1
    Label("Generated_Label_" + labelCount) 
  }

  def jump(s:S) = s match {
    case Label(name) => "jmp L1_" + name
    case _ => "jmp " + generateCode(s)
  }

  def jumpIfLess(l:Label) = "jl L1_" + l.l
  def jumpIfGreater(l:Label) = "jg L1_" + l.l
  def jumpIfEqual(l:Label) = "je L1_" + l.l

  def declare(l:Label) = "L1_" + l.l + ":"
}


/*
      (eax <- ebx < ecx)

    Here we need another trick; the x86 instruction set only let us
    update the lowest 8 bits with the result of a condition code. So,
    we do that, and then fill out the rest of the bits with zeros with
    a separate instruction:

      cmp %ecx, %ebx
      setl %al
      movzbl %al, %eax

   Here are the correspondances between the cx registers and the
   places where the condition codes can be stored:

    %eax's lowest 8 bits are %al
    %ecx's lowest 8 bits are %cl
    %edx's lowest 8 bits are %dl
    %ebx's lowest 8 bits are %bl
*/