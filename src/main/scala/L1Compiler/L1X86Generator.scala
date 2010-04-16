package L1Compiler

import L1AST._

object L1X86Generator{

  def generateCode(ast: L1): String = {
    header + generateMain(ast.main) + ast.funs.map(generateFunc).mkString + footer
  }
  def generateMain(main: L1Function) = {
    val footer = 	join("popl %ebp", "popl %edi", "popl %esi", "popl %ebx", "leave", "ret")
    main.body.map(i => "\t" + generateCode(i) + "\n").mkString + footer + "\n"
  }

  def generateFunc(f: L1Function) = {
    join( generateCode(f.name) + "\n" + f.body.map(i => "\t" + generateCode(i) + "\n").mkString )
  }

  private def join(s:String*) = s.mkString("\n\t")

  def generateCode(inst: Instruction): String = {
    inst match {
      case Num(n) => "$" + n
      case Label(l) => "$L1_" + l
      case r:Register => "%" + r.name
      case MemLoc(r, off) => off.n + "(" + generateCode(r) + ")"
      // TODO register assignment where s is a Comp
      case RegisterAssignment(r1, s) => "movl " + generateCode(s) + ", " + generateCode(r1)
      case MemWrite(loc, s) => "movl " + generateCode(s) + ", " + generateCode(loc)
      case MemRead(loc) => generateCode(loc)

      //edx += ecx  =>  addl %ecx, %edx
      case Increment(r, s) => "addl " + generateCode(s) + ", " + generateCode(r)
      //edx += ecx  =>  addl %ecx, %edx
      case Decrement(r, s) => "subl " + generateCode(s) + ", " + generateCode(r)
      // TODO: does this have to only work with eax somehow? 
      case Multiply(r, s) => "imull " + generateCode(s) + ", " + generateCode(r)

      case Print(s) => join("pushl " + generateCode(s), "call print", "addl $4,%esp")
      case Allocate(s, n) =>
        join("pushl " + generateCode(n), "pushl " + generateCode(s), "call allocate", "addl $8, %esp")
      case LabelDeclaration(l) => declare(l)
      case Goto(s) => jump(s)
      case Call(s) => {
        val label = nextNewLabel
        val jmp = s match { case Label(name) => name; case _ => generateCode(s) }
        join("pushl " + generateCode(label), "pushl %ebp", "movl %esp, %ebp", jump(s), declare(label))
      }

      case Comp(s:S, LessThan, s:S) => "cmpl " + generateCode(s) + ", " + generateCode(r)

      /////////// cjump < //////////

      // special case for two numbers
      case CJump(Comp(n1:Num, LessThan, n2:Num), l1, l2) => if(n1.n < n2.n) jump(l1) else jump(l2)
      // (cjump 11 < ebx :true :false) // special case. destination just be a register.
      case CJump(Comp(n:Num, LessThan, r:Register), l1, l2) =>
        join("cmpl " + generateCode(n) + ", " + generateCode(r), jumpIfGreater(l1), jump(l2))
      case CJump(comp, l1, l2) => join(generateCode(comp), jumpIfLess(l1), jump(l2))

      /////////// cjump == //////////
      
      case CJump(Comp(r:Register, EqualTo, s:S), l1, l2) =>
        join("cmpl " + generateCode(s) + ", " + generateCode(r), jumpIfEqual(l1), jump(l2))

      case Return => join("movl %ebp, %esp", "popl %ebp", "ret")
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

  def header =
""".file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl   %ebx
	pushl   %esi
	pushl   %edi
	pushl	%ebp
	movl	%esp, %ebp
"""

  def footer =
"""
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
"""
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