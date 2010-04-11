package L1Compiler

import reader._

/**
| (x += s)            ;; increment x by s
| (x -= s)            ;; decrement x by s
| (x *= s)            ;; multiply x by x
| (cx <- s cmp s)     ;; save result of a comparison
| (goto s)            ;; unconditional jump
| (cjump s cmp s label label) ;; conditional jump
| (call s)            ;; call a function [see below]
| (return)            ;; return from a function

And a few helper non-terminals:

s     ::= x | num | label
x     ::= cx | esi | edi | ebp | esp
cx    ::= eax | ecx | edx | ebx
cmp   ::= < | <= | =

num   ::= number between (inclusive) -2^31 and (2^31)-1
 **/
trait Exp
case class Num(n: Int) extends Exp
case class Label(l: String) extends Exp
abstract class Register(name: Symbol) extends Exp
object XRegister {
  def apply(s: Symbol): Option[XRegister] = s match {
    case 'esi => Some(esi)
    case 'edi => Some(edi)
    case 'ebp => Some(ebp)
    case 'esp => Some(esp)
    case _ => None
  }
}
sealed abstract case class XRegister(x: Symbol) extends Register(x)
object esi extends XRegister('esi)
object edi extends XRegister('edi)
object ebp extends XRegister('ebp)
object esp extends XRegister('esp)
object CXRegister {
  def apply(s: Symbol): Option[CXRegister] = s match {
    case 'eax => Some(eax)
    case 'ecx => Some(ecx)
    case 'edx => Some(edx)
    case 'ebx => Some(ebx)
    case _ => None
  }
}
sealed abstract case class CXRegister(cx: Symbol) extends Register(cx)
object eax extends CXRegister('eax)
object ecx extends CXRegister('ecx)
object edx extends CXRegister('edx)
object ebx extends CXRegister('ebx)
sealed abstract case class Comp(op: String) extends Exp
object LessThan extends Comp("<")
object LessThanOrEqualTo extends Comp("<=")
object EqualTo extends Comp("=")

case class RegisterAssignment(r: Register, s: Exp) extends Exp
case class MemLoc(basePointer: Register, offset: Num) extends Exp
case class MemRead(loc: MemLoc) extends Exp
case class MemWrite(loc: MemLoc, e:Exp) extends Exp

case class Print(e:Exp) extends Exp
case class Allocate(r:Register, init:Num) extends Exp

object L1Parser extends Parser[Exp] {
  def parse(expr: Any): Exp = {
    expr match {
      case s: Symbol => parseLabelOrRegister(s)
      case n: Int => Num(n)
      case List(x: Any, '<-, s: Any) => parseAssignment(x, s)
      case _ => error("unexpected token: " + expr)
    }
  }

  def parseLabelOrRegister(s: Symbol): Exp = {
    val chop = s.toString.drop(1) // remove the ' from ':label
    if (chop.startsWith(":")) parseLabel(chop) else parseRegister(s)
  }

  // label ::= sequence of alpha-numeric characters or underscore,
  // but starting with a colon, ie matching this regexp:
  // #rx"^:[a-zA-Z0-9_]$"
  def parseLabel(s: String) = {
    // TODO: put some legit error checking here...
    Label(s.drop(1)) // remove the : from :label.
  }

  def parseRegister(s: Symbol): Register = {
    XRegister(s).getOrElse(CXRegister(s).getOrElse(error(s + " is an invalid register")))
  }

  def parseAssignment(x: Any, s: Any) = {
    //println("parsing assignment of: " + x + " to: " + s)
    (x, '<-, s) match {
      //(x <- s) ;; assign to a register
      case (x: Symbol, '<-, i: Int) => RegisterAssignment(parseRegister(x), Num(i))
      case (x: Symbol, '<-, s: Symbol) => RegisterAssignment(parseRegister(x), parseRegister(s))
      // | (x <- (mem x n4))   ;; read from memory @ x+n4
      case (x1: Symbol, '<-, List('mem, x2:Symbol, n4:Int)) =>
        RegisterAssignment(parseRegister(x1), MemRead(MemLoc(parseRegister(x2), Num(n4))))
      // | ((mem x n4) <- s)   ;; update memory @ x+n4
      case (List('mem, x:Symbol, n4:Int), '<-, i: Int) =>
        MemWrite(MemLoc(parseRegister(x), Num(n4)), Num(i))
      case (List('mem, x:Symbol, n4:Int), '<-, s: Symbol) =>
        MemWrite(MemLoc(parseRegister(x), Num(n4)), parseRegister(s))
      // ;; two calls into the runtime system, one to print a value:
      //(eax <- (print s))
      // TODO: figure out if this is even legal (print 8)      
      case ('eax, '<-, List('print, i:Int)) => Print(Num(i))
      case ('eax, '<-, List('print, s:Symbol)) => Print(parseRegister(s))
      // ;; and one to allocate & initialize some space
      // (eax <- (allocate s s))
      case ('eax, '<-, List('allocate, s:Symbol, i:Int)) => Allocate(parseRegister(s), Num(i))
      case _ => error("bad assignment statement")
    }
  }
}
