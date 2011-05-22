package L4Compiler

import L4AST._
import L3Compiler.L3Printer
import L3Compiler.{L3AST => L3}

object L4CompilerMain extends L4Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = compileToString(new File(filename).read)
}

trait L4Compiler extends io.Reader with L4Parser with L4ToL3VConversions{

  def compile(code:String): L3.L3 = compile(parse(read(code)))
  def compile(ast:L4): L3.L3 = {
    L3.L3(find(changeVarNamesInE(ast.main)), ast.funs.map(f => find(changeVarNamesInFunc(f))))
  }
  def compileToString(code:String) = L3Printer.toCode(compile(code))

  trait Context
  case class LetContext(v:Variable, body:E, k:Context) extends Context
  case class IfContext(thenPart:E, elsePart:E, k:Context) extends Context
  case class FunCallContext(remainingEs:List[E], keyword: Option[Keyword], vs:List[L3.V], k:Context) extends Context
  case object NoContext extends Context

  def find(f:Func): L3.Func = L3.Func(f.label, f.args.map(convertVar), find(f.body, NoContext))
  def find(e:E): L3.E = find(e, NoContext)

  // find: L4-e context -> L3-e
  def find(e:E, k:Context): L3.E = e match {
    case Let(x, r, body) => find(r, LetContext(x, body, k))
    case IfStatement(c, tp, fp) => find(c, IfContext(tp, fp, k))
    case Begin(e1, e2) => find(Let(newVar(), e1, e2), k)
    case FunCall(kw:Keyword, Nil) => fill(kw.toD(Nil), k) // (new-tuple) case
    case FunCall(kw:Keyword, e::es) => find(e, FunCallContext(es, Some(kw), Nil, k))
    case FunCall(e:E, es) => find(e, FunCallContext(es, None, Nil, k))
    case v:V => fill(convertV(v), k)
  }

  // fill: L3-d context -> L3-e
  def fill(d:L3.D, k:Context): L3.E = k match {
    case LetContext(v, b, k) => L3.Let(v, d, find(b, k))
    case IfContext(t, e, k) => maybeLet(d, v => L3.IfStatement(v, find(t, k), find(e, k)))
    case fcc@FunCallContext(remainingEs, kw, vs, k) => remainingEs match {
      case (e::es) => maybeLet(d, v => find(e, fcc.copy(remainingEs=es, vs=vs:+v)))
      case Nil => kw match {
        case Some(kw) => maybeLet(d, v => fill(kw.toD(vs:+v), k))
        case None => maybeLet(d, v => {
          val vvs = vs :+ v
          fill(L3.FunCall(vvs.head, vvs.tail), k)
        })
      }
    }
    case NoContext => d
  }

  def maybeLet(d:L3.D, f: L3.V => L3.E): L3.E =
    if(d.isInstanceOf[L3.V]) f(d.asInstanceOf[L3.V]) else { val x = newVar(); L3.Let(x, d, f(x)) }

  private val count = Iterator.from(0)
  def newVar() = Variable("__x" + count.next())

  def changeVarNamesInFunc(f:Func): Func = {
    val newArgNames = f.args.map(x => newVar())
    Func(f.label, newArgNames, newNames(f.body, f.args.zip(newArgNames)))
  }
  def changeVarNamesInE(e:E, allowFrees:Boolean=false): E = newNames(e, Nil, allowFrees)
  def newNames(e:E, context:List[(Variable, Variable)], allowFrees:Boolean=false): E = {
    def inner(e:KeywordOrE, context:List[(Variable, Variable)]): E = e match {
      case v:Variable => context.find(vv => vv._1 == v) match {
        case Some(vv) => vv._2
        case None => if(allowFrees) v else error("free variable: " + v)
      }
      case Let(x, r, body) => {
        val newX = newVar()
        Let(newX, inner(r, context), inner(body, ((x, newX) :: context)))
      }
      case IfStatement(e1,e2,e3) => IfStatement(inner(e1, context),inner(e2, context),inner(e3, context))
      case Begin(e1,e2) => Begin(inner(e1, context),inner(e2, context))
      case FunCall(k:Keyword, args) => FunCall(k, args.map(e => inner(e, context)))
      case FunCall(k:E, args) => FunCall(inner(k, context), args.map(e => inner(e, context)))
      case k:Keyword => error("should have been caught in funcall...")
      case v:V => v // number or label. variable was handled above.
    }
    inner(e, context)
  }
}

trait L4ToL3VConversions {
  implicit def convertV(v:V) = v match {
    case v:Variable => convertVar(v)
    case n:Num => convertNum(n)
    case l:Label => convertLabel(l)
  }
  implicit def convertVar(v:Variable): L3.Variable = L3.Variable(v.name)
  implicit def convertNum(n:Num): L3.Num = L3.Num(n.n)
  implicit def convertLabel(l:Label): L3.Label = L3.Label(l.name)
}