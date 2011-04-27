package L3Compiler

object L3Printer extends L3Printer

trait L3Printer {
  import L3AST._
  def toCode(a:L3ASTNode): String = a match {
    case L3(main, funcs) => (main :: funcs).map(toCode).mkString("(", "\n", ")")
    //(l (x ...) e)
    case Func(label, args, body) =>
      "(" + toCode(label) + " " + args.map(toCode).mkString("(", " ", ")") + " " + toCode(body) + ")"

    // e ::= (let ([x d]) e) | (if v e e) | d
    case Let(x:X, d:D, body:E) => "(let ([" + toCode(x) + " " + toCode(d) + "]) " + toCode(body) + ")"
    case IfStatement(v:V, t:E, f:E) => "(if " + toCode(v) + " " + toCode(t) + " " + toCode(f) + ")"

    //biop ::= + | - | * | < | <= | =
    case Add(l:V, r:V)  => "(+ " + toCode(l) + " " +  toCode(r) + ")"
    case Sub(l:V, r:V)  => "(- " + toCode(l) + " " +  toCode(r) + ")"
    case Mult(l:V, r:V)  => "(* " + toCode(l) + " " +  toCode(r) + ")"
    case LessThan(l:V, r:V)  => "(< " + toCode(l) + " " +  toCode(r) + ")"
    case LessThanOrEqualTo(l:V, r:V)  => "(<= " + toCode(l) + " " +  toCode(r) + ")"
    case EqualTo(l:V, r:V)  => "(= " + toCode(l) + " " +  toCode(r) + ")"

    case IsNumber(v:V) => "(number? " + toCode(v) +")"
    case IsArray(v:V) => "(s? " + toCode(v) +")"
    case FunCall(v, args) => "("+ (v::args).map(toCode).mkString(" ") + ")"
    case NewArray(size:V, init:V) => "(new-array v v)"
    case NewTuple(vs) => "(new-tuple "+ vs.map(toCode).mkString(" ") + ")"
    case ARef(arr:V, loc:V) => "(aref " + toCode(arr) +" " + toCode(loc) +")"
    case ASet(arr:V, loc:V, newVal: V) => "(aset " + toCode(arr) +" " + toCode(loc) +" " + toCode(newVal) +")"
    case ALen(arr:V) => "(alen " + toCode(arr) +")"
    case MakeClosure(l:Label, v:V) => "(make-closure " + toCode(l) +" " + toCode(v) +")"
    case ClosureProc(v:V) => "(closure-proc " + toCode(v) +")"
    case ClosureVars(v:V) => "(closure-vars " + toCode(v) +")"
    case Print(v:V) => "(print " + toCode(v) + ")"

    case Num(n) => n.toString
    case Label(name: String) => ":" + name
    case v:Variable => v.name
  }
}