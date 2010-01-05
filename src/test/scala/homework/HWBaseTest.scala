package homework

class HWBaseTest[E,V](val interpreter:Interpreter[E,V] with Parser[E]) extends org.scalatest.FunSuite {

  def testCode(t: (String, V)): Unit = {
    test(t._1 + " mustBe " + t._2){
      assert(interpreter.eval(interpreter.parse(new Reader().read(t._1))) === t._2)
    }
  }

  def testExp(t: (E, V)): Unit = {
    println("calling test with: " + t)
    test(t._1 + " mustBe " + t._2){ assert(interpreter.eval(t._1) === t._2) }
  }
}