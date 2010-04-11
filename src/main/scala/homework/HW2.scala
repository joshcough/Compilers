package homework

import reader._

object HW2 {

  trait WAE
  case class Num(n:Int) extends WAE
  case class Id(name: Symbol) extends WAE
  case class Add(left: WAE, right: WAE) extends WAE
  case class Sub(left: WAE, right: WAE) extends WAE
  case class With(name: Symbol, named: WAE, body: WAE) extends WAE

  object HW2Interpreter extends Interpreter[WAE, Int] with Parser[WAE] {
    def eval(expr: WAE): Int = expr match {
      case Num(i) => i
      case Id(name) => error("unidentified id:" + name)
      case Add(l,r) => eval(l) + eval(r)
      case Sub(l,r) => eval(l) - eval(r)
      case With(id, named, body) => eval(subs(Num(eval(named)), id, body))
    }

    def subs(v:WAE, id:Symbol, expr:WAE): WAE = {
      expr match {
        case Num(i) => Num(i)
        case Id(name) => if(name == id) v else Id(name)
        case Add(l,r) => Add(subs(v, id, l), subs(v, id, r))
        case Sub(l,r) => Sub(subs(v, id, l), subs(v, id, r))
        case With(wid, named, body) =>
          if(wid == id) With(wid, subs(v, id, named), body)
          else With(wid, subs(v, id, named), subs(v,id,body))
      }
    }

    def parse(sexpr: Any): WAE = {
      sexpr match {
        case i: Int => Num(i)
        case s:Symbol => Id(s)
        case scala.List(Symbol("+"), xs, ys) => Add(parse(xs), parse(ys))
        case scala.List(Symbol("-"), xs, ys) => Sub(parse(xs), parse(ys))
        case scala.List('with, scala.List(sym:Symbol, xs), ys) => With(sym, parse(xs), parse(ys))
        case _ => error("unexpected token: " + sexpr)
      }
    }
  }

  def freeIds(wae:WAE): List[Symbol] = {
    checkIds(wae, (id, binders) => if(binders.contains(id.name)) Nil else List(id.name))
  }

  def boundIds(wae:WAE): List[Symbol] = {
    checkIds(wae, (id, binders) => if(binders.contains(id.name)) List(id.name) else Nil)
  }

  def checkIds(wae:WAE, idSeen: (Id, List[Symbol]) => List[Symbol]): List[Symbol] = {
    def checkIdsWithBinders(wae:WAE, binders: List[Symbol]): List[Symbol] = {
      val l = wae match {
        case Num(_) => Nil
        case i:Id => idSeen(i, binders)
        case Add(l,r) => checkIdsWithBinders(l, binders) ::: checkIdsWithBinders (r, binders)
        case Sub(l,r) => checkIdsWithBinders(l, binders) ::: checkIdsWithBinders (r, binders)
        case With(id, named, body) =>
          checkIdsWithBinders(named, binders) ::: checkIdsWithBinders (body, id :: binders)
      }
      l.toSet.toList.sort(_.toString < _.toString)
    }
    checkIdsWithBinders(wae, Nil)
  }

  def bindingIds(wae:WAE): List[Symbol] = {
    val l = wae match {
      case Num(_) => Nil
      case Id(_) => Nil
      case Add(l,r) => bindingIds(l) ::: bindingIds(r)
      case Sub(l,r) => bindingIds(l) ::: bindingIds(r)
      case With(id, named, body) => id :: bindingIds(named) ::: bindingIds(body)
    }
    l.toSet.toList.sort(_.toString < _.toString)
  }

}