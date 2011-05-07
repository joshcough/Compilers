package L4Compiler

object L4Printer extends L4Printer

trait L4Printer {
  import L4AST._
  def toCode(a:L4ASTNode): String = a match {
    case L4(main, funcs) => (main :: funcs).map(toCode).mkString("(", "\n", ")")
    //(l (x ...) e)
    case Func(label, args, body) =>
      "(" + toCode(label) + " " + args.map(toCode).mkString("(", " ", ")") + " " + toCode(body) + ")"

    // e ::= (let ([x d]) e) | (if v e e) | d
    case Let(v:Variable, e:E, body:E) => "(let ((" + toCode(v) + " " + toCode(e) + ")) " + toCode(body) + ")"
    case IfStatement(e:E, t:E, f:E) => "(if " + toCode(e) + " " + toCode(t) + " " + toCode(f) + ")"

    //biop ::= + | - | * | < | <= | =
    case Add(l:E, r:E)  => "(+ " + toCode(l) + " " +  toCode(r) + ")"
    case Sub(l:E, r:E)  => "(- " + toCode(l) + " " +  toCode(r) + ")"
    case Mult(l:E, r:E)  => "(* " + toCode(l) + " " +  toCode(r) + ")"
    case LessThan(l:E, r:E)  => "(< " + toCode(l) + " " +  toCode(r) + ")"
    case LessThanOrEqualTo(l:E, r:E)  => "(<= " + toCode(l) + " " +  toCode(r) + ")"
    case EqualTo(l:E, r:E)  => "(= " + toCode(l) + " " +  toCode(r) + ")"

    case Begin(l:E, r:E) => "(begin " +l+ " " +r+ ")"

    case IsNumber(e:E) => "(number? " + toCode(e) +")"
    case IsArray(e:E) => "(a? " + toCode(e) +")"
    case FunCall(v, args) => "("+ (v::args).map(toCode).mkString(" ") + ")"
    case NewArray(size:E, init:E) => "(new-array "+toCode(size)+" " + toCode(init) + ")"
    case NewTuple(vs) => "(new-tuple "+ vs.map(toCode).mkString(" ") + ")"
    case ARef(arr:E, loc:E) => "(aref " + toCode(arr) +" " + toCode(loc) +")"
    case ASet(arr:E, loc:E, newVal:E) => "(aset " + toCode(arr) +" " + toCode(loc) +" " + toCode(newVal) +")"
    case ALen(arr:E) => "(alen " + toCode(arr) +")"
    case MakeClosure(l:Label, e:E) => "(make-closure " + toCode(l) +" " + toCode(e) +")"
    case ClosureProc(e:E) => "(closure-proc " + toCode(e) +")"
    case ClosureVars(e:E) => "(closure-vars " + toCode(e) +")"
    case Print(e:E) => "(print " + toCode(e) + ")"

    case Num(n) => n.toString
    case Label(name: String) => ":" + name
    case v:Variable => v.name
  }
}