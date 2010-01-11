package homework

object HW1 {

  trait Exp
  case class StringLit(s:String) extends Exp
  case class Concat(left: Exp, right: Exp) extends Exp
  case class RestAfter(left: Exp, right: Exp) extends Exp

  object HW1Interpreter extends Interpreter[Exp, String] with Parser[Exp] {
    def eval(exp: Exp): String = {
      exp match {
        case StringLit(s) => s.drop(1).dropRight(1)
        case Concat(l, r) => eval(l) + eval(r)
        case RestAfter(l, r) => {
          val ls = eval(l)
          val rs = eval(r)
          val i = ls indexOf rs
          if (i == -1) error(rs + " not in " + ls) else ls.substring(i+1)
        }
      }
    }

    def parse(expr: Any): Exp = {
      expr match {
        case s: String => StringLit(s)
        case List(xs, Symbol("&"), ys) => Concat(parse(xs), parse(ys))
        case List(xs, Symbol("@"), ys) => RestAfter(parse(xs), parse(ys))
        case _ => error("unexpected token: " + expr)
      }
    }
  }
}
