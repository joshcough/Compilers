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
    val (main, extraFuncsFromMain) = find(changeVarNamesInE(ast.main))
    val (funs, moreFuns) = ast.funs.map(f => find(changeVarNamesInFunc(f))).unzip
    L3.L3(main, extraFuncsFromMain ::: funs ::: moreFuns.flatten)
  }
  def compileToString(code:String) = L3Printer.toCode(compile(code))

  trait Context
  case class LetContext(v:Variable, body:E, k:Context) extends Context
  case class IfContext(thenPart:E, elsePart:E, k:Context) extends Context
  case class FunCallContext(remainingEs:List[E], keyword: Option[Keyword], vs:List[L3.V], k:Context) extends Context
  case object NoContext extends Context

  def find(f:Func): (L3.Func, List[L3.Func]) = {
    val (fBody, extraFunsFromFBody) = find(f.body, NoContext)
    (L3.Func(f.label, f.args.map(convertVar), fBody), extraFunsFromFBody)
  }
  def find(e:E): (L3.E, List[L3.Func]) = find(e, NoContext)

  // find: L4-e context -> L3-e
  def find(e:E, k:Context): (L3.E, List[L3.Func]) = e match {
    case Let(x, r, body) => find(r, LetContext(x, body, k))
    case IfStatement(c, tp, fp) => find(c, IfContext(tp, fp, k))
    case Begin(e1, e2) => find(Let(newVar(), e1, e2), k)
    case FunCall(kw:Keyword, Nil) => fill(kw.toD(Nil), k) // (new-tuple) case
    case FunCall(kw:Keyword, e::es) => find(e, FunCallContext(es, Some(kw), Nil, k))
    case FunCall(e:E, es) => find(e, FunCallContext(es, None, Nil, k))
    case v:V => fill(convertV(v), k)
  }

  // fill: L3-d context -> L3-e
  def fill(d:L3.D, k:Context): (L3.E, List[L3.Func]) = k match {
    case NoContext => (d, Nil)
    case LetContext(v, b, k) => {
      val (letBody, extraFuncs) = find(b, k)
      (L3.Let(v, d, letBody), extraFuncs)
    }
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
    /**
     * (+ (if v e_1 e_2) e_big) =>
     *   (let ((ctxt
     *     (lambda (ret-val) (+ ret-val e_big))))
     *       (if v (ctxt e_1) (ctxt e_2)))
     **/
    case IfContext(t, e, k) => maybeLet(d, v => {
      val fLabel = newLabel()
      val fArg = newVar()
      val (fBody, extraFuncsFromFBody) = fill(fArg, k)
      val freeVariables = freeVars(fBody).filterNot(_==convertVar(fArg))
      val freesTup = L3.Variable("frees")
      val fbodyWithFrees = freeVariables.zipWithIndex.foldRight(fBody){
        case ((v,i), b) => L3.Let(v, L3.ARef(freesTup, L3.Num(i)), b)
      }
      val func = L3.Func(fLabel, List(fArg, freesTup), fbodyWithFrees)
      val tup = NewTuple(freeVariables.map(l3Var2L4Var))
      val (tt, tef) = find(FunCall(fLabel, List(t, tup)), NoContext)
      val (ee, eef) = find(FunCall(fLabel, List(e, tup)), NoContext)
      (L3.IfStatement(v, tt, ee), func :: (extraFuncsFromFBody ::: tef ::: eef))
    })
  }

  def maybeLet(d:L3.D, f: L3.V => (L3.E, List[L3.Func])): (L3.E, List[L3.Func]) =
    if(d.isInstanceOf[L3.V]) f(d.asInstanceOf[L3.V]) else {
      val x = newVar()
      val (letBody, extraFuns) = f(x)
      (L3.Let(x, d, letBody), extraFuns)
    }

  private val varCount = Iterator.from(0)
  def newVar() = Variable("__x" + varCount.next())
  private val labelCount = Iterator.from(0)
  def newLabel() = Label("__f" + labelCount.next())


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

  def freeVars(e:L3.E): List[L3.Variable] = {
    def inner(e:L3.E, bound:List[L3.Variable]): List[L3.Variable] = e match {
      case v:L3.Variable => bound.find(_==v) match { case None => List(v); case _ => Nil}
      case L3.Let(x, r, body) => inner(r, bound) ::: inner(body, x :: bound)
      case L3.IfStatement(e, t, f) => inner(e, bound) ::: inner(t, bound) ::: inner(f, bound)
      case b:L3.Biop => inner(b.left, bound) ::: inner (b.right, bound)
      case L3.IsNumber(v) => inner(v, bound)
      case L3.IsArray(v) => inner(v, bound)
      case L3.FunCall(v, args) => inner(v, bound) ::: args.flatMap(a => inner(a, bound))
      case L3.NewArray(size, init) => inner(size, bound) ::: inner(init, bound)
      case L3.NewTuple(vs) => vs.flatMap(v => inner(v, bound))
      case L3.ARef(arr, loc) => inner(arr, bound) ::: inner(loc, bound)
      case L3.ASet(arr, loc, newVal: V) => inner(arr, bound) ::: inner(loc, bound) ::: inner(newVal, bound)
      case L3.ALen(arr) => inner(arr, bound)
      case L3.MakeClosure(l, v) => inner(v, bound)
      case L3.ClosureProc(v) => inner(v, bound)
      case L3.ClosureVars(v) => inner(v, bound)
      case L3.Print(v) => inner(v, bound)
      case L3.Num(n) => Nil
      case L3.Label(name) => Nil
    }
    inner(e, Nil).distinct
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

  implicit def l3Var2L4Var(v:L3.Variable) = Variable(v.name)
}
