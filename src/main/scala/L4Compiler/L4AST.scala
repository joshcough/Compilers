package L4Compiler

import L3Compiler.{L3AST => L3}

object L4AST extends FunctionLifters {

  object L4 { def apply(main: E): L4 = L4(main, Nil) }
  case class L4(main: E, funs: List[Func])
  case class Func(label: Label, args: List[Variable], body: E)
  sealed trait KeywordOrE
  sealed trait KeywordOrV extends KeywordOrE
  sealed trait E extends KeywordOrE

  case class Keyword(name:String, toD: List[L3.V] => L3.D) extends KeywordOrE with KeywordOrV

  val keywords@List(add, sub, mult, lt, lteq, eq, num_?, a_?,
    newarr, newtup, aref, aset, alen, print, makeclosure, closureproc, closurevars) = List(
    Keyword("+", lift2(L3.Add(_, _))),
    Keyword("-", lift2(L3.Sub(_, _))),
    Keyword("*", lift2(L3.Mult(_, _))),
    Keyword("<", lift2(L3.LessThan(_, _))),
    Keyword("<=", lift2(L3.LessThanOrEqualTo(_, _))),
    Keyword("=", lift2(L3.EqualTo(_, _))),
    Keyword("number?", lift1(L3.IsNumber(_))),
    Keyword("a?", lift1(L3.IsArray(_))),
    Keyword("new-array", lift2(L3.NewArray(_, _))),
    Keyword("new-tuple", (vs => L3.NewTuple(vs))),
    Keyword("aref", lift2(L3.ARef(_, _))),
    Keyword("aset", lift3(L3.ASet(_, _, _))),
    Keyword("alen", lift1(L3.ALen(_))),
    Keyword("print", lift1(L3.Print(_))),
    Keyword("make-closure", lift2((l, v) => L3.MakeClosure(l.asInstanceOf[L3.Label], v))),
    Keyword("closure-proc", lift1(L3.ClosureProc(_))),
    Keyword("closure-vars", lift1(L3.ClosureVars(_))))
  val keywordsMap: Map[String, Keyword] = keywords.map(k => (k.name, k)).toMap

  def ARef(arr:E, index:E) = FunCall(aref, arr, index)
  def IsArray(arr:E) = FunCall(a_?, arr)
  def MakeClosure(l:Label, e:E) = FunCall(makeclosure, l, e)
  def NewTuple(es:List[E]) = FunCall(newtup, es)
  def ClosureProc(e:E) = FunCall(closureproc, e)
  def ClosureVars(e:E) = FunCall(closurevars, e)

  case class Let(v: Variable, e: E, body: E) extends E
  case class Begin(e1: E, e2: E) extends E
  case class IfStatement(e: E, truePath: E, falsePath: E) extends E {
    def es = List(e, truePath, falsePath);
    def rebuild = lift3(IfStatement.apply _)
  }
  object FunCall{ def apply(f:KeywordOrE, args: E*) = new FunCall(f, args.toList) }
  case class FunCall(f: KeywordOrE, args: List[E]) extends E
  trait V extends E with KeywordOrV
  case class Variable(name: String) extends V
  case class Num(n: Int) extends V
  case class Label(name: String) extends V
}

trait FunctionLifters {
  def lift1[T, U](f: T => U): (List[T]) => U = (ts: List[T]) => ts match {
    case x :: Nil => f(x); case _ => error("expected 1 arg, but got: " + ts)
  }
  def lift2[T, U](f: (T, T) => U): (List[T]) => U = (ts: List[T]) => ts match {
    case x :: y :: Nil => f(x, y); case _ => error("expected 2 args, but got: " + ts)
  }
  def lift3[T, U](f: (T, T, T) => U): (List[T]) => U = (ts: List[T]) => ts match {
    case x :: y :: z :: Nil => f(x, y, z); case _ => error("expected 3 args, but got: " + ts)
  }
}