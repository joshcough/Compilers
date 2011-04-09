package reader

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import io.Reader

class ReaderTest extends FunSuite with MustMatchers {

  // primitivate cases
  testRead("0", 0)
  testRead("1", 1)
  testRead("-1", -1)
  testRead("2147483647", 2147483647)
  testRead("-2147483648", -2147483648)
  testRead("'g'", 'g')
  testRead("'1'", '1')  // this is the character 1, not the number
  testRead("2x2", Symbol("2x2"))
  testRead("hello", 'hello) // unquoted strings are symbols.
  testRead("\"hello\"", "\"hello\"") // quoted strings are strings.
  testRead("\"hello world\"", "\"hello world\"")

  // list cases
  testRead("(hey world)", List('hey, 'world))
  testRead("(\"hey\" world)", List("\"hey\"", 'world))
  testRead("(esi <- -1)", List('esi, '<-, -1))

  // just add a bunch of white space to the last test
  testRead(" (  \"hey\"    world   )   ", List("\"hey\"", 'world))
  // nested list cases
  testRead("(hey (hey world) world)", List('hey, List('hey, 'world), 'world))
  testRead("(hey (hey world) world 1 (1 2))",
    List('hey, List('hey, 'world), 'world, 1, List(1, 2)))

  // error cases
  testRead("'aa'", Error("unclosed character literal"))
  testRead("'a", Error("unclosed character literal"))
  testRead("\"a", Error("unclosed string literal"))
  testRead("(a", Error("unclosed list"))
  testRead(")", Error("unexpected list terminator"))

  testRead("""(
  :aint_gonna_happen
  :terminate)""", List(Symbol(":aint_gonna_happen"), Symbol(":terminate")))

  testRead(""";;10
(((eax <- 19)
  (eax <- (print eax))))""", List(List(List('eax, '<-, 19), List('eax, '<-, List('print, 'eax)))))


  // helper functions
  def read(s:String) = new Reader{}.read(s)
  case class Error(message:String)
  def testRead(s: String, a: Any) = test(s + " => " + a){ read(s) must be(a) }
  def testRead(s:String,e:Error) = {
    test(s + " => " + e){ intercept[Throwable]{ read(s) }.getMessage must be(e.message) }
  }
}