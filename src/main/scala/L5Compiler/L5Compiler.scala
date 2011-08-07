package L5Compiler

import L5AST._
import L4Compiler.{ L4AST => L4 }
import L4Compiler.L4Printer
import LCompiler.LNCompiler

/**
 * L5Compiler is a lambda lifter (http://en.wikipedia.org/wiki/Lambda_lifting).
 *
 * L5 is the language being compiled. It contains one top level expression only.
 * The grammar for L5 can be found in the same directory as this file, L5Grammar.txt.
 *
 * The L5 compiler produces L4 expressions. One top expression ('main') and
 * a list of L4 procedures. These procedures are the lambdas in the L5 expression
 * lifted out to be top level procedures.
 *
 * The L4 grammar can be found in the L4Compiler directory, a sibling of
 * the L5Compiler directory where this file (L5Compiler.scala) lives.
 */
trait L5Compiler extends LNCompiler with L5Parser with L5Printer {
  //  L5 -> L4 Implicit Conversions
  implicit def convertVar(v:Variable) = L4.Variable(v.name)
  implicit def convertVarList(vs:List[Variable]) = vs.map(convertVar)

  /**
   * The main compile function.
   *
   * The function works by matching on the type of e,
   * one case for each type of e (from the grammar).
   *
   * The first several cases, Num, Var, If, Begin, NewTuple, and Let
   * are all trivial cases, transforming directly to the same L4 version.
   * The remainder of the cases are each documented in place.
   *
   * @param e - the L5 expression to compile
   * @return - a tuple containing one top level L4 expression and a list of L4 functions
   *           the functions are the lifted lambdas. 
   */
  def compile(e:E): (L4.E, List[L4.Func]) = e match {
    case Num(n) => (L4.Num(n), Nil)
    case v:Variable => (v, Nil)
    case IfStatement(e, t, f) =>
      compileSubExpressions(e,t,f){ es => L4.IfStatement(es(0), es(1), es(2)) }
    case Begin(e1, e2) =>
      compileSubExpressions(e1,e2){ es => L4.Begin(es(0), es(1)) }
    case NewTuple(es) =>
      compileSubExpressions(es:_*){ es => L4.NewTuple(es) }
    case Let(x, e1, e2) =>
      compileSubExpressions(e1,e2){ es => L4.Let(x, es(0), es(1)) }

    /**
     * letrec doesn't exist in L4, so we do this simple transformation.
     * (letrec ([x e1]) e2)
     *  =>
     * (let ([x (new-tuple 0)])
     *   (begin (aset x 0 e1[x:=(aref x 0)])
     *          e2[x:=(aref x 0)]))
     */
    case LetRec(x, e1, e2) => compile(
      Let(x, NewTuple(List(Num(0))),
        Begin(
          App(ASet, List(x, Num(0), sub(x, App(ARef, List(x, Num(0))), e1))),
          sub(x, App(ARef, List(x, Num(0))), e2)
        )
      )
    )

    /**
     * We Turn (f +) => (f (lambda (x y) (+ x y)))
     * So when we see a primitive function by itself,
     * turn it into a lambda expression, and then compile that expression.
     */
    case p:Prim => compile(Lambda(p.vars, App(p, p.vars)))

    /**
     * Here is where we actually do the lambda lifting.
     *
     * The general tranformation looks a bit like this:
     *
     *  (lambda (x ...) e) => (make-closure :f (new-tuple y1 y2 ... y-n))
     *  where (y1 y2 ... y-n) are the free variables in (lambda (x ...) e),
     *  and we create a new procedure:
     *  (:f (vars-tup x ...)
     *    (let ([y1 (aref vars-tup 0)])
     *      (let ([y2 (aref vars-tup 1)])
     *        ...
     *          (let ([y-n (aref vars-tup n)])
     *             e))))
     *
     * At the application site, we replace the lambda with a closure.
     * The closure contains the name of the new top level function
     * and all of the free variables in the body of the lambda.
     *
     * The body of the lambda gets yanked out into the top level function.
     *
     * The first argument of the new function is the tuple (array)
     * containing the values of the free variables in the original lambda.
     *
     * L4 functions can only have three arguments. So we may need to do one
     * more transformation. If the lambda itself had two or fewer formal args,
     * then we can simply fit them in. But if it had 3 or more, we can't fit
     * them in (because we have one extra argument now for the free vars)
     * In this case, we create a new tuple for them as well and so the function
     * will take two tuple arguments, one for the free vars and one for
     * the remainder of the arguments.
     *
     * For the free vars tuple, we have to transform the body of the function
     * to first get the free vars out of the tuple, using the same name name
     * as the original argument. For example if we have (lambda (x) y)
     * then the first thing we do in the new function is (let ([y (aref vars-tup 0)])
     * One let each of the free vars.
     *
     * If we need to use a tuple for the remaining arguments, we create lets
     * in the very same way.
     */
    case Lambda(args, body) => {
      val usingArgsTuple = args.size > 2
      val (freesVar, argsVar) = (L4.Variable("frees"), L4.Variable("args"))
      // the arguments to the new function
      val fArgs: List[L4.Variable] =
        if(usingArgsTuple) List(freesVar, argsVar) else freesVar :: args.map(convertVar)
      // the free variables in the lambda
      val frees = freeVars(e)
      // the body for the new function
      val (fBody, moreFunctions) = {
        // compile the body of the lambda
        val (compiledLambdaBody, funcs) = compile(body)
        // then wrap it with the required let statements
        val lets = {
          def wrapWithLets(tup: L4.Variable, vars: List[L4.Variable], e: L4.E) =
            vars.zipWithIndex.foldRight(e) {
              case ((v, i), b) => L4.Let(v, L4.ARef(tup, L4.Num(i)), b)
            }
          val freeLets = wrapWithLets(freesVar, frees, compiledLambdaBody)
          if(! usingArgsTuple) freeLets else wrapWithLets(argsVar, args, freeLets)
        }
        (lets, funcs)
      }
      val label = newLabel()
      // finally, the closure, and the new function
      // (and any other functions lifted from compiling the body of the lambda)
      (L4.MakeClosure(label, L4.NewTuple(frees)), L4.Func(label, fArgs, fBody) :: moreFunctions)
    }

    /**
     * Primitive function application
     * (+ x y z)  => (+ x y z)
     * also a trivial case, but I wanted to keep this case close to the other App case.
     */
    case App(p:Prim, args) =>
      compileSubExpressions(args:_*){ es => L4.FunCall(L4.keywordsMap(p.name), es) }

    /**
     * In this case, we know we don't have a primitive function
     * in the function position, so we must have a closure created
     * from the Lambda case.
     *
     * (e0 e1 ... en) =>
     * (let ([f e0]) ((closure-proc f) (closure-vars f) e1 ... en))
     */
    case App(f, args) => {
      val v = newVar()
      // compile the function position
      val (compiledF, extraFunctions) = compile(f)
      // compile all of the arguments
      val (compiledArgs, moreExtraFunctions) = compileEs(args)
      //
      (L4.Let(v, compiledF,
        // free variables go in the first argument.
        L4.FunCall(L4.ClosureProc(v), L4.ClosureVars(v) ::
        // if we can fit the rest of the arguments in, then great
        // if not, they must also go into another tuple.
        (if (compiledArgs.size <= 2) compiledArgs else List(L4.NewTuple(compiledArgs))))),
      extraFunctions ::: moreExtraFunctions)
    }
  }

  /**
   * This function is used to compile the subexpressions of and L5 e.
   * Once they are compiled, we need to construct the equivalent L4 e.
   * using the freshly compiled subexpressions.
   *
   * For example, an L5 if-statement has three subexpressions
   * They get compiled into three L4 expressions (and some lifted functions)
   * which in turn, get shoved into an L4 if-statement.
   *
   * @param es  The subexpressions to compile
   * @param createL4E  A function that takes the compiled subexpressions and
   *   creates the L4 expression as explained above
   * @return  a tuple containing one L4 expression and a list of L4 functions
   *          the functions are the lambdas lifted when compiling the es.
   */
  def compileSubExpressions(es:E*)(createL4E: List[L4.E] => L4.E) = {
    val (l4es, fs) = compileEs(es.toList)
    (createL4E(l4es), fs)
  }

  /**
   * Compile each of the given L5 expressions
   *
   * @param es  A list of L5 es to be compiled
   * @return  a tuple containing
   *    a list of L4 es, one e for each of the incoming L5 es
   *    a list of L4 functions. the functions are the lambdas lifted when compiling the es.
   */
  def compileEs(es:List[E]): (List[L4.E], List[L4.Func]) = {
    val (l4es, fs) = es.map(compile).unzip
    (l4es, fs.flatten)
  }

  // TODO: In L4, I renamed all the variables in the program to avoid
  // possible name collisions with f and x. This needs to be done for L5.
  private val labelCount = Iterator.from(0)
  private val varCount = Iterator.from(0)
  private def newLabel() = L4.Label("f" + labelCount.next())
  private def newVar() = L4.Variable("x" + varCount.next())

  /**
   * Find all the free vars in an expression.
   * @param e  The expression
   * @return a list which contains all the free variables in e
   */
  def freeVars(e:E): List[Variable] = {
    def inner(e:E, bound:List[Variable]): List[Variable] = e match {
      case Lambda(args, body) => inner(body, args ::: bound)
      case v:Variable => bound.find(_==v) match { case None => List(v); case _ => Nil}
      case Let(x, r, body) => inner(r, bound) ::: inner(body, x :: bound)
      case LetRec(x, r, body) => inner(r, x :: bound) ::: inner(body, x :: bound)
      case IfStatement(e, t, f) => inner(e, bound) ::: inner(t, bound) ::: inner(f, bound)
      case Begin(e1, e2) => inner(e1, bound) ::: inner(e2, bound)
      case NewTuple(es) => es.flatMap(e => inner(e, bound))
      case App(f, args) => inner(f, bound) ::: args.flatMap(e => inner(e, bound))
      case _ => Nil
    }
    inner(e, Nil).distinct
  }

  /**
   * Replace all occurrences of x for y in e.
   * @param x The variable to be replaced
   * @param y The expression to replace x with
   * @param e The expression to do the replacing in
   * @return A new expression with all x's replaced by y
   */
  def sub(x:Variable, y:E, e:E): E = {
    def inner(e:E): E = e match {
      case Lambda(args, body) => Lambda(args, if(args.contains(x)) body else inner(body))
      case v:Variable => if(v==x) y else v
      case Let(v, e1, body) => Let(v, inner(e1), if(v==x) body else inner(body))
      case LetRec(v, e1, body) => LetRec(v, if(v==x) e1 else inner(e1), if(v==x) body else inner(body))
      case IfStatement(e, t, f) => IfStatement(inner(e), inner(t), inner(f))
      case Begin(e1, e2) => Begin(inner(e1), inner(e2))
      case NewTuple(es) => NewTuple(es.map(inner))
      case App(f, args) => App(inner(f), args.map(inner))
      case _ => e
    }
    inner(e)
  }

  // Two simple utility functions.

  /**
   * Compile the given code to L4, and return it in String representation.
   */
  def compileToString(code:String) = L4Printer.toCode(compile(code))

  /**
   * Compile the given code.
   * First, call read on it (to turn it into an s-expression)
   * Then parse it into an L5 AST, then call the main compilation
   * funtion with that AST as the argument.
   */
  def compile(code:String): L4.L4 = {
    val (e,fs) = compile(parse(read(code)))
    L4.L4(e, fs)
  }
}