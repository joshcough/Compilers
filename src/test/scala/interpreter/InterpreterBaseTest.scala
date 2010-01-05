package interpreter

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

trait LazyInterpreterBaseTest extends InterpreterBaseTest {
  import interpreter.LazyInterpreter._
  type Exp = LazyInterpreter.Exp
  type Value = LazyInterpreter.Value
  def doEval(expr:Exp): Value = strict(eval(expr))
}

trait InterpreterBaseTest extends FunSuite with MustMatchers{  
  type Exp
  type Value

  def test(t: (Exp, Value)): Unit = test("", t)

  def test(name: String, t: (Exp, Value)): Unit = {
    test((name + " " + t._1 + " mustBe " + t._2).trim){
      doEval(t._1) must be(t._2)
    }
  }

  implicit def anyToMustBe(a:Any) = new {
    def mustBe(b:Any){
      a must be(b)
    }
  }

  def doEval(expr:Exp): Value
}