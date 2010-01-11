package homework

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class ReaderTest extends FunSuite with MustMatchers {

  // primitivate cases
  testRead("1", 1)
  testRead("'g'", 'g')
  testRead("'1'", '1')
  testRead("hello", 'hello)
  testRead("\"hello\"", "\"hello\"")
  testRead("\"hello world\"", "\"hello world\"")

  // list cases
  testRead("(hey world)", List('hey, 'world))
  testRead("(\"hey\" world)", List("\"hey\"", 'world))
  // just add a bunch of white space to the last test
  testRead(" (  \"hey\"    world   )   ", List("\"hey\"", 'world))

  // error cases
  testRead("'aa'", Error("unclosed character literal"))
  testRead("'a", Error("unclosed character literal"))
  testRead("\"a", Error("unclosed string literal"))
  testRead("(a", Error("unclosed list"))
  testRead(")", Error("unexpected list terminator"))

  // helper functions 
  def read(s:String) = new Reader().read(s)
  case class Error(message:String)
  def testRead(s: String, a: Any) = test(s + " => " + a){ read(s) must be(a) }
  def testRead(s:String,e:Error) = {
    test(s + " => " + e){ intercept[Throwable]{ read(s) }.getMessage must be(e.message) }
  }
}