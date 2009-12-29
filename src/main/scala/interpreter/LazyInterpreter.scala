package interpreter

object LazyInterpreter {

  type Name = Symbol
  type Env = List[(Name, Value)]

  trait Exp
  case class Num(value: Int) extends Exp
  case class Id(value: Name) extends Exp
  case class Add(left: Exp, right: Exp) extends Exp
  case class Sub(left: Exp, right: Exp) extends Exp
  case class Func(id: Name, body: Exp) extends Exp
  case class Application(f: Exp, arg: Exp) extends Exp

  trait Value
  case class NumValue(value: Int) extends Value
  case class Closure(param: Name, body: Exp, env: Env) extends Value
  case class ExpClosure(expr: Exp, env: Env) extends Value

  def eval(exp: Exp): Value = eval(exp, Nil)

  def eval(exp: Exp, env: Env): Value = {
    exp match {
      case Num(v) => NumValue(v)
      case Id(v) => lookup(v, env)
      case Add(l, r) => strictMath(eval(l, env), eval(r, env), _ + _)
      case Sub(l, r) => strictMath(eval(l, env), eval(r, env), _ - _)
      case Func(id, body) => Closure(id, body, env)
      case Application(f, arg) => strict(eval(f, env)) match {
        case Closure(p,b,e) => eval(b, (p, ExpClosure(arg, env)) :: e)
        case bad => error("expected function in function position of application, but found: " + bad)
      }
    }
  }

  def lookup(id:Name, env: Env): Value = env.find { nv => nv._1 == id }.getOrElse(error("unbound id: " + id))._2

  def strict(value: Value): Value = value match {
    case ExpClosure(exp, env) => strict(eval(exp, env))
    case _ => value
  }

  def strictMath(l:Value, r: Value, f: (Int, Int) => Int): NumValue = {
    (strict(l), strict(r)) match {
      case (NumValue(a), NumValue(b)) => NumValue(a + b)
      case _ => error("something other than num passed to math function")
    }
  }
}