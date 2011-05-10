package L4Compiler

import L4AST._

trait L4Parser { this: L4Compiler =>

  //p ::= (e (l (x ...) e) ...)
  def parse(exp:Any): L4 = exp match {
    case e :: funcs => new L4(main=parseE(e), funcs map parseFunction)
    case _ => error("bad L4 program: " + exp)
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
    case 'let :: rest => parseLet(rest)
    case 'if :: testE :: thenE :: elseE :: Nil => IfStatement(parseE(testE), parseE(thenE), parseE(elseE))
    case '+  :: left :: right :: Nil => Add(parseE(left), parseE(right))
    case '-  :: left :: right :: Nil => Sub(parseE(left), parseE(right))
    case '*  :: left :: right :: Nil => Mult(parseE(left), parseE(right))
    case '<  :: left :: right :: Nil => LessThan(parseE(left), parseE(right))
    case '<= :: left :: right :: Nil => LessThanOrEqualTo(parseE(left), parseE(right))
    case '=  :: left :: right :: Nil => EqualTo(parseE(left), parseE(right))
    case Symbol("number?") :: e :: Nil => IsNumber(parseE(e))
    case Symbol("a?") :: e :: Nil => IsArray(parseE(e))
    case 'begin :: e1 :: e2 :: Nil => Begin(e1=parseE(e1), e2=parseE(e2))
    case Symbol("new-array") :: s :: e :: Nil => NewArray(size = parseE(s), init = parseE(e))
    case Symbol("new-tuple") :: xs => NewTuple(xs map parseE)
    case 'aref :: arr :: loc :: Nil => ARef(arr=parseE(arr), loc=parseE(loc))
    case 'aset :: arr :: loc :: e :: Nil => ASet(arr=parseE(arr), loc=parseE(loc), newVal=parseE(e))
    case 'alen :: arr :: Nil => ALen(arr=parseE(arr))
    case 'print :: e :: Nil => Print(e=parseE(e))

    case Symbol("make-closure") :: (l:Symbol) :: e :: Nil => MakeClosure(l=parseLabel(l.toString), e=parseE(e))
    case Symbol("closure-proc") :: e :: Nil => ClosureProc(e=parseE(e))
    case Symbol("closure-vars") :: e :: Nil => ClosureVars(e=parseE(e))

    case n: Int => Num(n)
    case s: Symbol => if (s.toString.startsWith("':")) parseLabel(s.toString) else parseVar(s)

    case e :: es => FunCall(parseE(e), es map parseE)
  }

  def parseLet(rest:List[Any]) = {
    val argList = rest.head.asInstanceOf[List[Any]].head.asInstanceOf[List[Any]]
    val x = parseVar(argList.head.asInstanceOf[Symbol])
    val e1 = parseE(argList.last)
    val body = parseE(rest.tail.head)
    Let(x, e1, body)
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