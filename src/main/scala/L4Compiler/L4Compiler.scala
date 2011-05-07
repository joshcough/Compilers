package L4Compiler

import L4AST._

trait L4Compiler extends io.Reader with L4Parser with L4Printer {

  def compileE(code:String) = L4Printer.toCode(find(parseE(read(code)), NoContext))

  def isV(e:E): Boolean = e.isInstanceOf[V]

  trait Context
  case class LetContext(v:Variable, body:E, k:Context) extends Context
  case class IfContext(thenPart:E, elsePart:E, k:Context) extends Context
  case class FunContext(args:List[E], k:Context) extends Context
  case class ArgContext(f:V, k:Context) extends Context
  case class PrintContext(k:Context) extends Context
  case class AddLeftContext(r:E, k:Context) extends Context
  case class AddRightContext(l:V, k:Context) extends Context
  case class SubLeftContext(r:E, k:Context) extends Context
  case class SubRightContext(l:V, k:Context) extends Context
  case class MultLeftContext(r:E, k:Context) extends Context
  case class MultRightContext(l:V, k:Context) extends Context
  case class LessThanLeftContext(r:E, k:Context) extends Context
  case class LessThanRightContext(l:V, k:Context) extends Context
  case class LessThanEqLeftContext(r:E, k:Context) extends Context
  case class LessThanEqRightContext(l:V, k:Context) extends Context
  case class EqLeftContext(r:E, k:Context) extends Context
  case class EqRightContext(l:V, k:Context) extends Context
  case object NoContext extends Context

  // NOTE: kind of terrible that the return val has to be an L4 E here...
  // find: L4-e context -> L3-e
  def find(e:E, k:Context): E = e match {
    case FunCall(f, args) => find(f, FunContext(args, k))
    case Let(x, r, body) => find(r, LetContext(x, r, k))
    case IfStatement(c, tp, fp) => find(c, IfContext(tp, fp, k))
    case Print(e) => find(e, PrintContext(k))
    case Add(l, r) => find(l, AddLeftContext(r, k))
    case Sub(l, r) => find(l, SubLeftContext(r, k))
    case Mult(l, r) => find(l, MultLeftContext(r, k))
    case LessThan(l, r) => find(l, LessThanLeftContext(r, k))
    case LessThanOrEqualTo(l, r) => find(l, LessThanEqLeftContext(r, k))
    case EqualTo(l, r) => find(l, EqLeftContext(r, k))
    case v:V => fill(e, k)
    // TODO: other cases here too... lots.
    case _ =>  error("implement me")
  }

  // NOTE: kind of terrible that the first arg has to be an L4 E here...
  // NOTE: kind of terrible that the return val has to be an L4 E here...
  // fill: L3-d context -> L3-e
  def fill(d:E, k:Context): E = k match {
    case LetContext(v, b, k) => Let(v, d, find(b, k))
    case IfContext(t, e, k) => next(d, k, v => IfStatement(v, find(t, k), find(e, k)))
    // TODO: only handling one argument.
    case FunContext(as, k) => next(d, k, v => find(as.head, ArgContext(v, k)))
    // TODO: i think this is all busted because a single argument
    // could have more after it. maybe we need a new context type like
    // LastArgContext which would do this work, and ... something that
    // comes before LastArg...
    // TODO obviously List(e) doesnt really work here. see comment above.
    case ArgContext(f, k) => next(d, k, v => fill(FunCall(f, List(v)), k))
    case PrintContext(k) => next(d, k, v => fill(Print(v), k))
    case AddLeftContext(r, k) => next(d, k, v => find(r, AddRightContext(v, k)))
    case AddRightContext(l, k) => next(d, k, v => fill(Add(l, v), k))
    case SubLeftContext(r, k) => next(d, k, v => find(r, SubRightContext(v, k)))
    case SubRightContext(l, k) => next(d, k, v => fill(Sub(l, v), k))
    case MultLeftContext(r, k) => next(d, k, v => find(r, MultRightContext(v, k)))
    case MultRightContext(l, k) => next(d, k, v => fill(Mult(l, v), k))
    case LessThanLeftContext(r, k) => next(d, k, v => find(r, LessThanRightContext(v, k)))
    case LessThanRightContext(l, k) => next(d, k, v => fill(LessThan(l, v), k))
    case LessThanEqLeftContext(r, k) => next(d, k, v => find(r, LessThanEqRightContext(v, k)))
    case LessThanEqRightContext(l, k) => next(d, k, v => fill(LessThanOrEqualTo(l, v), k))
    case EqLeftContext(r, k) => next(d, k, v => find(r, EqRightContext(v, k)))
    case EqRightContext(l, k) => next(d, k, v => fill(EqualTo(l, v), k))
    case NoContext => d
  }

  def next(d:E, k:Context, f: V => E): E = {
    if(isV(d)) f(d.asInstanceOf[V])
    else {
      val x = newVar()
      Let(x, d, f(x))
    }
  }

  private val count = Iterator.from(0)
  def newVar() = Variable("__x" + count.next())
}

//import L3Compiler.L3AST.{E => L3E, D, Let => L3Let, Variable => L3Variable, isV}

//trait L4ToL3Implicits {
//  implicit def convertVar(v:Variable): L3Variable = L3Variable(v.name)
////  implicit def convertNum(n:Num): L2Num = L2Num(n.n)
////  implicit def convertLabel(l:Label): L2Label = L2Label(l.name)
////  implicit def convertX(x:X) = x match { case v: Variable => convertVar(v) }
////  implicit def convertVToS(v:V): S = v match {
////    case Num(n) => L2Num(n)
////    case v:Variable => convertVar(v)
////    case Label(name) => L2Label(name)
////  }
////  def declare(l:Label) = LabelDeclaration(L2Label(l.name))
//}

//  def isD(e:E): Boolean = e match {
//    case Let(x, e, body) => vs(e, body)
//    case IfStatement(e, truePath, falsePath) => isV(e)
//    case biop:Biop => vs(biop.left, biop.right)
//    case Begin(l, r) => vs(l,r)
//    case IsNumber(e) => isV(e)
//    case IsArray(e) => isV(e)
//    case FunCall(e, args) => vs((e :: args):_*)
//    case NewArray(size, init) => vs(size, init)
//    case NewTuple(tupleVs) => vs(tupleVs:_*)
//    case ARef(arr, loc) => vs(arr, loc)
//    case ASet(arr, loc, newVal) => vs(arr, loc, newVal)
//    case ALen(arr) => isV(arr)
//    case Print(e) => isV(e)
//    case MakeClosure(l, e) => isV(e)
//    case ClosureProc(e) => isV(e)
//    case ClosureVars(e) => isV(e)
//    case Variable(name) => true
//    case Num(n) => true
//    case Label(name) => true
//  }
//
//  def vs(es:E*) = ! es.find(e => ! isV(e)).isDefined
//


//  def nextV(e:E): Boolean = e match {
//    case l@Let(x, e, body) => if(isV())
//    case i@IfStatement(e, truePath, falsePath) => isV(e)
//    case biop:Biop => vs(biop.left, biop.right)
//    case b@Begin(l, r) => vs(l,r)
//    case i@IsNumber(e) => isV(e)
//    case i@IsArray(e) => isV(e)
//    case f@FunCall(e, args) => vs((e :: args):_*)
//    case n@NewArray(size, init) => vs(size, init)
//    case n@NewTuple(tupleVs) => vs(tupleVs:_*)
//    case a@ARef(arr, loc) => vs(arr, loc)
//    case a@ASet(arr, loc, newVal) => vs(arr, loc, newVal)
//    case a@ALen(arr) => isV(arr)
//    case p@Print(e) => isV(e)
//    case m@MakeClosure(l, e) => isV(e)
//    case c@ClosureProc(e) => isV(e)
//    case c@ClosureVars(e) => isV(e)
//    case Variable(name) => true
//    case Num(n) => true
//    case Label(name) => true
//  }