package interpreter

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

trait LazyInterpreterBaseTest extends InterpreterBaseTest {
  import interpreter.LazyInterpreter._
  def doEval(expr:Expression): Value = strict(eval(expr))
}

trait InterpreterBaseTest extends FunSuite with MustMatchers{

  import interpreter.LazyInterpreter._

  def test(t: (Expression, Value)): Unit = test("", t)

  def test(name: String, t: (Expression, Value)): Unit = {
    test((name + " " + t._1 + " mustBe " + t._2).trim){
      doEval(t._1) must be(t._2)
    }
  }

  implicit def anyToMustBe(a:Any) = new {
    def mustBe(b:Any){
      a must be(b)
    }
  }

  def doEval(expr:Expression): Value
}