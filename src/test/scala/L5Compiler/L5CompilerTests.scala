package L5Compiler

import util.TestHelpers
import L5AST._

class L5CompilerTests extends TestHelpers {

  testFreeVariables("(lambda (x) (x (y z)))", List("y", "z"))
  testFreeVariables("(lambda (x) (lambda (y) (y z)))", List("z"))
  testFreeVariables("(lambda () (x (y z)))", List("x", "y", "z"))
  testFreeVariables("x", List("x"))
  testFreeVariables("7", Nil)
  testFreeVariables("(let ((x 6)) x)", Nil)
  testFreeVariables("(let ((x y)) x)", List("y"))
  testFreeVariables("(let ((y 6)) x)", List("x"))
  testFreeVariables("(let ((y 6)) (x (y z)))", List("x", "z"))
  testFreeVariables("(if 1 7 8)", Nil)
  testFreeVariables("(if 1 7 z)", List("z"))
  testFreeVariables("(if 1 y 8)", List("y"))
  testFreeVariables("(if x y z)", List("x", "y", "z"))
  testFreeVariables("(let ((y 6)) (if 1 y 7))", Nil)
  testFreeVariables("(begin 1 2)", Nil)
  testFreeVariables("(begin 1 y)", List("y"))
  testFreeVariables("(begin x 2)", List("x"))
  testFreeVariables("(begin x y)", List("x", "y"))
  testFreeVariables("(new-tuple x 2 3 4 5 6 y)", List("x", "y"))
  testFreeVariables("(x y)", List("x", "y"))
  //TODO case LetRec(x, r, body) => inner(r, x :: bound) ::: inner(body, x :: bound)


  def testFreeVariables(e:String, expected:List[String]){
    test("free variables in: " + e){
      val compiler = new L5Compiler{}
      import compiler._
      println(parse(read(e)))
      expect(freeVars(parse(read(e))).mkString(" "))(expected.mkString(" "))
    }
  }
}
