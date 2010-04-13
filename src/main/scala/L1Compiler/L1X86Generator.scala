package L1Compiler

import L1AST._

object L1X86Generator{

  def generateCode(ast: L1): String = {
    header + generateMain(ast.main) + ast.funs.map(generateFunc).mkString + footer
  }

  def generateMain(main: L1Function) = main.body.map(i => "\t" + generateCode(i) + "\n").mkString

  def generateFunc(f: L1Function) = {
    ""
  }

  def generateCode(inst: Instruction): String = {

    def join(s:String*) = s.mkString("\n\t")

    inst match {
      case RegisterAssignment(r1, r2:Register) => "movl " + genCode(r2) + ", " + genCode(r1)
      case RegisterAssignment(r, n:Num) => "movl " + genCodeForS(n) + ", " + genCode(r)
      //edx += ecx  =>  addl %ecx, %edx
      case Increment(r, s) => "addl " + genCodeForS(s) + ", " + genCode(r)
      //edx += ecx  =>  addl %ecx, %edx
      case Decrement(r, s) => "subl " + genCodeForS(s) + ", " + genCode(r)
      case Print(s) => join("pushl " + genCodeForS(s), "call print", "addl $4,%esp")
      case Allocate(s, n) => join("pushl " + genCodeForS(n), "pushl " + genCodeForS(s), "call allocate", "addl $8, %esp")
    /**
     * (cjump eax < ebx :true :false)
      =>
      cmpl %ebx, %eax   // note reversal of argument order here.
      jl _true
      jmp _false
     */
    }
  }

  private def genCode(r:Register) = "%" + r.name

  private def genCodeForS(s:S) = s match {
    case Num(n) => "$" + n
    case r:Register => genCode(r)
    case Label(l) => "$" + l
  }

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
	popl   %ebp
	popl   %edi
	popl   %esi
	popl   %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
"""

}
