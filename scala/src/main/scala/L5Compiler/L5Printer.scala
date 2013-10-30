package L5Compiler

object L5Printer extends L5Printer

trait L5Printer {
  import L5AST._
  def toCode(e:E): String = e match {
    case Lambda(args, body) => "(lambda " + args.mkString("(", " ", ")") + " " + toCode(body) + ")"
    case v:Variable => v.name
    case Let(v:Variable, e:E, body:E) => "(let ((" + toCode(v) + " " + toCode(e) + ")) " + toCode(body) + ")"
    case LetRec(v:Variable, e:E, body:E) => "(letrec ((" + toCode(v) + " " + toCode(e) + ")) " + toCode(body) + ")"
    case IfStatement(e:E, t:E, f:E) => "(if " + toCode(e) + " " + toCode(t) + " " + toCode(f) + ")"
    case NewTuple(vs) => "(new-tuple "+ vs.map(toCode).mkString(" ") + ")"
    case Begin(l:E, r:E) => "(begin " +toCode(l)+ " " +toCode(r)+ ")"
    case App(e, args) => "("+ (e::args).map(toCode).mkString(" ") + ")"
    case Num(n) => n.toString
    case p:Prim => p.keyword.name
  }
}