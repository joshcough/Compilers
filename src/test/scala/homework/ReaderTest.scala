package homework

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class ReaderTest extends FunSuite with MustMatchers {

  testRead("hello" -> "hello")
  testRead("\"hello\"" -> "\"hello\"")
  testRead("\"hello world\"" -> "\"hello world\"")

  testRead("(hey world)" -> List("hey", "world"))
  testRead("(\"hey\" world)" -> List("\"hey\"", "world"))

  def testRead(t: (String,Any)){
    test(t._1 + " must be " + t._2 ){
      new Reader().read(t._1.toStream) must be(t._2)
    }
  }
}