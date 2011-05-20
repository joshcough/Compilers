package L4Compiler

import L4AST._

object L4CompilerMain extends L4Compiler {
  import java.io.File
  import io.FileHelper._
  def main(args:Array[String]) = println(compileFile(args(0)))
  def compileFile(filename:String): String = compileToString(new File(filename).read)
}

trait L4Compiler extends io.Reader with L4Parser with L4Printer {

  def compile(code:String): L4 = compile(parse(read(code)))
  def compile(ast:L4) = {
    L4(find(changeVarNamesInE(ast.main)), ast.funs.map(f => find(changeVarNamesInFunc(f))))
  }
  def compileToString(code:String) = L4Printer.toCode(compile(code))

  trait Context
  case class LetContext(v:Variable, body:E, k:Context) extends Context
  case class IfContext(thenPart:E, elsePart:E, k:Context) extends Context
  case class ENContext(remainingEs:List[E], vs:List[V], f: List[V] => E, k:Context) extends Context
  case object NoContext extends Context

  def find(f:Func): Func = f.copy(body=find(f.body))
  def find(e:E): E = find(e, NoContext)

  // find: L4-e context -> L3-e
  def find(e:E, k:Context): E = e match {
    case Let(x, r, body) => find(r, LetContext(x, body, k))
    case IfStatement(c, tp, fp) => find(c, IfContext(tp, fp, k))
    case Begin(e1, e2) => find(Let(newVar(), e1, e2), k)
    case en: EN => en.es match {
      case e :: es => find(e, ENContext(es, Nil, en.rebuild, k))
      case Nil => fill(en, k) // for empty (new-tuple) expressions.
    }
    case v:V => fill(e, k)
  }

  // fill: L3-d context -> L3-e
  def fill(d:E, k:Context): E = k match {
    case LetContext(v, b, k) => Let(v, d, find(b, k))
    case IfContext(t, e, k) => maybeLet(d, v => IfStatement(v, find(t, k), find(e, k)))
    case enc@ENContext(remainingEs, vs, rebuild, k) => remainingEs match {
      case (e::es) => maybeLet(d, v => find(e, enc.copy(remainingEs=es, vs=vs :+ v)))
      case Nil => maybeLet(d, k, v => fill(rebuild(vs :+ v), k))
    }
    case NoContext => d
  }

  def maybeLet(d:E, f: V => E): E =
    if(d.isInstanceOf[V]) f(d.asInstanceOf[V]) else { val x = newVar(); Let(x, d, f(x)) }

  private val count = Iterator.from(0)
  def newVar() = Variable("__x" + count.next())

  def changeVarNamesInFunc(f:Func): Func = {
    val newArgNames = f.args.map(x => newVar())
    Func(f.label, newArgNames, newNames(f.body, f.args.zip(newArgNames)))
  }
  def changeVarNamesInE(e:E, allowFrees:Boolean=false): E = newNames(e, Nil, allowFrees)
  def newNames(e:E, context:List[(Variable, Variable)], allowFrees:Boolean=false): E = {
    def inner(e:E, context:List[(Variable, Variable)]): E = e match {
      case v:Variable => context.find(vv => vv._1 == v) match {
        case Some(vv) => vv._2
        case None => if(allowFrees) v else error("free variable: " + v)
      }
      case Let(x, r, body) => {
        val newX = newVar()
        Let(newX, inner(r, context), inner(body, ((x, newX) :: context)))
      }
      case en: EN =>  en.rebuild(en.es.map(e => inner(e, context)))
      case v:V => v // number or label. variable was handled above.
    }
    inner(e, context)
  }
}