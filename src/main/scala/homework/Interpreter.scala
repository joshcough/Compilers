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
      if(stream.isEmpty) error("unterminated list")
      else stream head match {
        case ')' => (acc, stream.tail)
        case _ => {
          val (next, rest) = readWithRest(stream)
          readList(rest, acc ::: List(next))
        }
      }
    }

    def readChars(stream:Stream[Char], acc: String): (String, Stream[Char]) = {
      if(stream.isEmpty) (acc, stream)
      else stream head match {
        case '(' | ')' | ' ' => (acc, stream)
        case c:Char => readChars(stream.tail, acc + c)
      }
    }

    def readStringLit(stream: Stream[Char], acc: String): (String, Stream[Char]) = {
      if(stream.isEmpty) error("unterminated string literal")
      else stream head match {
        case '"' => (acc + '"', stream.tail)
        case c:Char => readStringLit(stream.tail, acc + c)
      }
    }

    stream head match {
      case '(' => readList(stream.tail, Nil)
      case ' ' => readWithRest(stream.tail)
      case '"' => readStringLit(stream.tail, "\"")
      case ')' => error("unexpected token )")
      case c:Char => readChars(stream.tail, c.toString)
    }
  }
}