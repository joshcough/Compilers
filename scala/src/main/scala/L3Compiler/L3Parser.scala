package L3Compiler

import L3AST._

trait L3Parser {

  //p ::= (e (l (x ...) e) ...)
  def parse(exp:Any): L3 = exp match {
    case e :: funcs => new L3(main=parseE(e), funcs map parseFunction)
    case _ => error("bad L3 program: " + exp)
  }

  //(l (x ...) e)
  def parseFunction(exp: Any): Func = exp match {
    case (lab:Symbol) :: (args:List[Any]) :: e :: Nil =>
      Func(parseLabel(lab.toString), parseArgs(args), parseE(e))
    case _ => error("bad function: " + exp)
  }

  def parseArgs(args:List[Any]) = {
    if(args.isInstanceOf[List[Symbol]])
      args.asInstanceOf[List[Symbol]].map((s:Symbol) => Variable(s.toString.drop(1)))
    else error("bad argument list!")
  }

  //e ::= (let ([x d]) e) | (if v e e) | d
  def parseE(a:Any): E = a match {
    case 'let :: List(List((x:Symbol), d)) :: e :: Nil => Let(parseVar(x), parseD(d), parseE(e))
    case 'if :: v :: le :: re :: Nil => IfStatement(parseV(v), parseE(le), parseE(re))
    case _ => parseD(a)
  }

  //d ::= (biop v v)   where biop ::= + | - | * | < | <= | =
  //      (pred v)     where pred ::= number? | a? ;; a? tests to see if the argument is an array or a tuple
  //      (v v ...)
  //      (new-array v v)
  //      (new-tuple v ...)
  //      (aref v v)
  //      (aset v v v)
  //      (alen v)
  //      (print v)
  //      (make-closure l v)
  //      (closure-proc v)
  //      (closure-vars v)
  //      v where v :: = x | l | num
  def parseD(a:Any): D = a match {
    case '+  :: left :: right :: Nil => Add(parseV(left), parseV(right))
    case '-  :: left :: right :: Nil => Sub(parseV(left), parseV(right))
    case '*  :: left :: right :: Nil => Mult(parseV(left), parseV(right))
    case '<  :: left :: right :: Nil => LessThan(parseV(left), parseV(right))
    case '<= :: left :: right :: Nil => LessThanOrEqualTo(parseV(left), parseV(right))
    case '=  :: left :: right :: Nil => EqualTo(parseV(left), parseV(right))
    case Symbol("number?") :: v :: Nil => IsNumber(parseV(v))
    case Symbol("a?") :: v :: Nil => IsArray(parseV(v))

    case Symbol("new-array") :: s :: v :: Nil => NewArray(size = parseV(s), init = parseV(v))
    case Symbol("new-tuple") :: xs => NewTuple(xs map parseV)
    case 'aref :: arr :: loc :: Nil => ARef(arr=parseV(arr), loc=parseV(loc))
    case 'aset :: arr :: loc :: v :: Nil => ASet(arr=parseV(arr), loc=parseV(loc), newVal=parseV(v))
    case 'alen :: arr :: Nil => ALen(arr=parseV(arr))
    case 'print :: v :: Nil => Print(v=parseV(v))

    case Symbol("make-closure") :: (l:Symbol) :: v :: Nil => MakeClosure(l=parseLabel(l.toString), v=parseV(v))
    case Symbol("closure-proc") :: v :: Nil => ClosureProc(v=parseV(v))
    case Symbol("closure-vars") :: v :: Nil => ClosureVars(v=parseV(v))

    case n: Int => Num(n)
    case s: Symbol => parseLabelOrVar(s)

    case v :: vs => FunCall(parseV(v), vs map parseV)
    case _ => parseV(a)
  }
  
  def parseV(exp:Any) = exp match {
    case n: Int => Num(n)
    case s: Symbol => parseLabelOrVar(s)
  }

  def parseNumOrVar(exp:Any): V = exp match {
    case n: Int => Num(n)
    case s: Symbol => parseLabelOrVar(s)
  }

  def parseLabelOrVar(s: Symbol): V = {
    if (s.toString.startsWith("':")) parseLabel(s.toString) else parseVar(s)
  }

  // label ::= sequence of alpha-numeric characters or underscore,
  // but starting with a colon, ie matching this regexp:
  // #rx"^:[a-zA-Z0-9_]$"
  def parseLabel(s: String) = {
    val chop = s.toString.drop(1) // remove the ' from ':label
    Label(s.drop(2)) // remove the ' and : from ':label.
  }

  def parseVar(s: Symbol): Variable = Variable(s.toString.drop(1))
}