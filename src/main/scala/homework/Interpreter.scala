package homework

trait Interpreter[E,V] extends Parser[E]{
  def eval(exp: E): V
}

trait Parser[E] {
  def parse(exp: Any): E
}

class Reader {

  def read(s:String): Any = read(s.toStream)

  def read(stream:Stream[Char]): Any = readWithRest(stream)._1

  def readWithRest(stream:Stream[Char]): (Any, Stream[Char]) = {

    def readList(stream: Stream[Char], acc: List[Any]): (List[Any], Stream[Char]) = {
      stream match {
        case ')' #:: tail => (acc, tail)
        case x   #:: tail =>
          val (next, rest) = readWithRest(stream)
          readList(rest, acc ::: List(next))
	case Stream()     => error("unterminated list")
      }
    }

    def readChars(stream:Stream[Char]): (String, Stream[Char]) = {
      val (chars, rest) = stream.span( ! List('(', ')', ' ').contains(_) ) 
      (chars.mkString, rest)
    }

    def readStringLit(stream: Stream[Char], acc: String): (String, Stream[Char]) = {
      stream match {
        case '"' #:: tail => (acc + '"', tail)
        case c   #:: tail => readStringLit(tail, acc + c)
	case Stream()     => error("unterminated string literal")
      }
    }

    stream match {
      case '(' #:: tail => readList(tail, Nil)
      case ' ' #:: tail => readWithRest(tail)
      case '"' #:: tail => readStringLit(tail, "\"")
      case ')' #:: _    => error("unexpected token )")
      case _ => readChars(stream)
    }
  }
}
