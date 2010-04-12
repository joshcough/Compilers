package L1Compiler

import L1AST._

object L1X86Generator{

  def generateCode(exp: L1) = {
    
  }

  def generateCode(inst: Instruction) = {
    inst match {
      //edx += ecx  =>  addl %ecx, %edx
      case Increment(r, s) => "addl " + genCodeForS(s) + ", " + genCode(r)

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
}