package L4Compiler

object L4Printer extends L4Printer

trait L4Printer {
  import L4AST._

  def toCode(l4:L4): String = (toCode(l4.main) :: l4.funs.map(toCode)).mkString("(", "\n", ")")
  def toCode(f:Func): String =
    "(" + toCode(f.label) + " " + f.args.map(toCode).mkString("(", " ", ")") + " " + toCode(f.body) + ")"
  def toCode(a:KeywordOrE): String = a match {
    case Let(v:Variable, e:E, body:E) => "(let ([" + toCode(v) + " " + toCode(e) + "]) " + toCode(body) + ")"
    case IfStatement(e:E, t:E, f:E) => "(if " + toCode(e) + " " + toCode(t) + " " + toCode(f) + ")"
    case Begin(e1:E, e2:E) => "(begin " + toCode(e1) + " " + toCode(e2) + ")"
    case FunCall(v, args) => "("+ (toCode(v)::args.map(toCode)).mkString(" ") + ")"
    case Num(n) => n.toString
    case Label(name: String) => ":" + name
    case v:Variable => v.name
    case k:Keyword => k.name
  }
}