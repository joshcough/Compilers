package homework

import reader._
import io.Reader

abstract class HWBaseTest[E,V](val interpreter:Interpreter[E,V] with Parser[E]) extends org.scalatest.FunSuite {

  def read(s:String): Any = new Reader{}.read(s)
  def parse(a:Any) = interpreter parse a
  def eval(e:E):V = interpreter eval e

  def testCode(t: (String, V)): Unit = {
    test(t._1 + " => " + t._2){
      assert(interpreter.eval(parse(read(t._1))) === t._2)
    }
  }

  def testExp(t: (E, V)): Unit = {
    println("calling test with: " + t)
    test(t._1 + " => " + t._2){ assert(eval(t._1) === t._2) }
  }

  def simpleTest[T,U](t: (T, U)): Unit = {
    test(t._1 + " => " + t._2){ assert(t._2 === t._1) }
  }

}
