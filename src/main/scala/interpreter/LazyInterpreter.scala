package interpreter

object LazyInterpreter {

  type Name = Symbol
  type Environment = List[(Name, Value)]

  trait Expression
  case class Num(value: Int) extends Expression
  case class Id(value: Name) extends Expression
  case class Add(left: Expression, right: Expression) extends Expression
  case class Sub(left: Expression, right: Expression) extends Expression
  case class Func(id: Name, body: Expression) extends Expression
  case class Application(f: Expression, arg: Expression) extends Expression

  trait Value
  case class NumValue(value: Int) extends Value
  case class Closure(param: Name, body: Expression, env: Environment) extends Value
  case class ExpClosure(expr: Expression, env: Environment) extends Value

  def eval(expr: Expression): Value = eval(expr, Nil)

  def eval(expr: Expression, env: Environment): Value = {
    expr match {
      case Num(v) => NumValue(v)
      case Id(v) => lookup(v, env)
      case Add(l, r) => strictMath(eval(l, env), eval(r, env), _ + _)
      case Sub(l, r) => strictMath(eval(l, env), eval(r, env), _ - _)
      case Func(id, body) => Closure(id, body, env)
      case Application(f, arg) => {
        strict(eval(f, env)) match {
          case Closure(p,b,e) => eval(b, (p, ExpClosure(arg, env)) :: e)
          case bad => error("expected function in function position of application, but found: " + bad)
        }
      }
    }
  }

  def lookup(id:Name, env: Environment): Value = env match {
    case (name, value)::xs => if(name == id) value else lookup(id, xs)
    case _ => error("unbound id: " + id)
  }

    def strict(value: Value): Value = {
    value match {
      case ExpClosure(expr, env) => strict(eval(expr, env))
      case _ => value
    }
  }

  def strictMath(l:Value, r: Value, f: (Int, Int) => Int): NumValue = {
    (strict(l), strict(r)) match {
      case (NumValue(a), NumValue(b)) => NumValue(a + b)
      case _ => error("something other than num passed to strictAdd")
    }
  }
}