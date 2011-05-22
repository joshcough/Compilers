package L5Compiler

import L5AST._

trait L5Parser {

  def parse = parseE _

  def parseE(a:Any): E = a match {
    case 'lambda :: rest => parseLambda(rest)
    case 'let :: rest => parseLet(rest, Let.apply _)
    case 'letrec :: rest => parseLet(rest, LetRec.apply _)
    case 'if :: testE :: thenE :: elseE :: Nil => IfStatement(parseE(testE), parseE(thenE), parseE(elseE))
    case Symbol("new-tuple") :: xs => NewTuple(xs map parseE)
    case 'begin :: e1 :: e2 :: Nil => Begin(e1=parseE(e1), e2=parseE(e2))
    case e :: es => App(parseE(e), es map parseE)
    case '+ => Add
    case '- => Sub
    case '* => Mult
    case '< => LessThan
    case '<= => LessThanOrEqualTo
    case '= => EqualTo
    case Symbol("number?") => IsNumber
    case Symbol("a?") => IsArray
    case Symbol("new-array") => NewArray
    case 'aref => ARef
    case 'aset => ASet
    case 'alen => ALen
    case 'print => Print
    case n: Int => Num(n)
    case s: Symbol => parseVar(s)
  }

  def parseLet(rest:List[Any], f: (Variable, E, E) => E): E = {
    val argList = rest.head.asInstanceOf[List[Any]].head.asInstanceOf[List[Any]]
    val x = parseVar(argList.head.asInstanceOf[Symbol])
    val e1 = parseE(argList.last)
    val body = parseE(rest.tail.head)
    f(x, e1, body)
  }

  def parseLambda(rest: List[Any]): Lambda = {
    def parseArgs(args: List[Any]) = {
      if (args.isInstanceOf[List[Symbol]])
        args.asInstanceOf[List[Symbol]].map((s: Symbol) => Variable(s.toString.drop(1)))
      else error("bad argument list!")
    }
    Lambda(parseArgs(rest.head.asInstanceOf[List[Any]]), parseE(rest.tail.head))
  }

  def parseVar(s:Symbol) = Variable(s.toString.drop(1))
}