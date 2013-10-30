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
    case 'begin :: e1 :: e2 :: elseE => Begin(parseE(e1), parseE(e2))
    case e :: es => FunCall(parseKeywordOrE(e), (es map parseE):_*)
    case n: Int => Num(n)
    case s: Symbol => if (s.toString.startsWith("':")) parseLabel(s.toString) else parseVar(s)
  }

  def parseKeywordOrE(a:Any): KeywordOrE = a match {
    case s: Symbol => if (s.toString.startsWith("':")) parseLabel(s.toString) else parseKeywordOrVar(s)
    case _ => parseE(a)
  }

  def parseLet(rest:List[Any]) = {
    val argList = rest.head.asInstanceOf[List[Any]].head.asInstanceOf[List[Any]]
    val x = parseVar(argList.head.asInstanceOf[Symbol])
    val e1 = parseE(argList.last)
    val body = parseE(rest.tail.head)
    Let(x, e1, body)
  }

  def parseLabel(s: String) = {
    val chop = s.toString.drop(1) // remove the ' from ':label
    Label(s.drop(2)) // remove the ' and : from ':label.
  }

  def parseKeywordOrVar(s: Symbol) = keywordsMap.getOrElse(s.toString.drop(1), parseVar(s))
  def parseVar(s: Symbol): Variable = Variable(s.toString.drop(1))
}