package reader

trait Interpreter[E,V] {
  def eval(exp: E): V
}

trait Parser[E] {
  def parse(exp: Any): E
}

trait Reader {

  def read(s:String): Any = read(stripComments(s).toStream)
  def stripComments(code:String) = code.split("\n").map(s => s.takeWhile(_!=';').trim).mkString(" ")

  def read(stream:Stream[Char]): Any = readWithRest(stream)._1

  def readWithRest(s:String): (Any, Stream[Char]) = readWithRest(s.toStream)
  def readWithRest(stream:Stream[Char]): (Any, Stream[Char]) = {

    def readList(stream: Stream[Char], acc: List[Any]): (List[Any], Stream[Char]) = {
      stream match {
        case ' ' #:: tail => readList(tail, acc)
        case ')' #:: tail => (acc, tail)
        case x   #:: tail =>
          val (next, rest) = readWithRest(stream)
          readList(rest, acc ::: List(next))
	      case Stream()     => error("unclosed list")
      }
    }

    def readSymbol(stream:Stream[Char]): (Symbol, Stream[Char]) = {
      val (chars, rest) = stream.span( ! List('(', ')', ' ', '\n').contains(_) )
      (Symbol(chars.mkString), rest)
    }

    def readNumOrMaybeSymbol(stream:Stream[Char], negate:Boolean): (Any, Stream[Char]) = {
      val (chars, rest) = stream.span( ! List('(', ')', ' ', '\n').contains(_) )
      // if there are any non number characters, this must be a symbol
      if(chars.exists(c => ! Character.isDigit(c))) (Symbol(chars.mkString), rest)
      else {
        val n = chars.mkString.toInt
        (if(negate) -n else n, rest)
      }
    }

    def readStringLit(stream: Stream[Char], acc: String): (String, Stream[Char]) = {
      stream match {
        case '"' #:: tail => (acc + '"', tail)
        case c   #:: tail => readStringLit(tail, acc + c)
	      case Stream()     => error("unclosed string literal")
      }
    }

    def readCharLit(stream: Stream[Char]): (Char, Stream[Char]) = {
      stream match {
        case c #:: '\'' #:: tail => (c, tail)
	      case _  => error("unclosed character literal")
      }
    }

    stream match {
      case '(' #:: tail => readList(tail, Nil)
      case ' ' #:: tail => readWithRest(tail)
      case '\n' #:: tail => readWithRest(tail)
      case '"' #:: tail => readStringLit(tail, "\"")
      case '\'' #:: tail => readCharLit(tail)
      case ')' #:: _    => error("unexpected list terminator")
      case c #:: tail if(Character.isDigit(c)) => {
        readNumOrMaybeSymbol(stream, negate=false)
      }
      case '-' #:: c #:: tail if(Character.isDigit(c)) => {
        readNumOrMaybeSymbol(c #:: tail, negate=true)
      }
      case _ => readSymbol(stream)
    }
  }
}
