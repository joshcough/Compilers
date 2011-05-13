package L5Compiler

import util.TestHelpers
import L5AST._
import L4Compiler.L4Printer

class L5CompilerTests extends TestHelpers {

  testCompile("7", "(7)")

  // one arg, no frees
  testCompile("(lambda (x) x)", """
((make-closure :f0 1)
(:f0 (x) x))""")

  // one arg, one free
  testCompile("(lambda (x) y)", """
((make-closure :f0 (new-tuple y))
(:f0 (frees x) (let ((y (aref frees 0))) y)))
""")

  // two args, one free
  // (we need frees, because there are free vars, but we can fit the rest in as args)
  testCompile("(lambda (x y) z)", """
((make-closure :f0 (new-tuple z))
(:f0 (frees x y) (let ((z (aref frees 0))) z)))
""")

  // three args, none free (dont need frees or args!)
  testCompile("(lambda (a b c) a)", """
((make-closure :f0 1)
(:f0 (a b c) a))
""")

  // three args, one free (we need both frees and args here.)
  testCompile("(lambda (a b c) z)", """
((make-closure :f0 (new-tuple z))
(:f0 (frees args) (let ((a (aref args 0))) (let ((b (aref args 1))) (let ((c (aref args 2))) (let ((z (aref frees 0))) z))))))
""")

  // four args, none free (over the limit, but notice we dont have 'frees', just 'args')
  testCompile("(lambda (a b c d) a)", """
((make-closure :f0 1)
(:f0 (args) (let ((a (aref args 0))) (let ((b (aref args 1))) (let ((c (aref args 2))) (let ((d (aref args 3))) a))))))
""")

  // inner lambda
  testCompile("(lambda (x) (lambda (y) y))", """
((make-closure :f0 1)
(:f0 (x) (make-closure :f1 1))
(:f1 (y) y))
""")

  testCompile("(+ 5 6)", "((+ 5 6))")

  testCompile("(+ 5 (lambda (x) x))", """
((+ 5 (make-closure :f0 1))
(:f0 (x) x))""")

  // no frees in the lambda. one arg.
  testCompile("((lambda (x) x) 5)", """
((let ((x0 (make-closure :f1 1))) (if (number? (closure-vars x0)) ((closure-proc x0) 5) ((closure-proc x0) (closure-vars x0) 5)))
(:f1 (x) x))""")

  // no frees in the lambda. two args.
  testCompile("((lambda (x y) x) 5 6)", """
((let ((x0 (make-closure :f1 1))) (if (number? (closure-vars x0)) ((closure-proc x0) 5 6) ((closure-proc x0) (closure-vars x0) 5 6)))
(:f1 (x y) x))""")

  // no frees in the lambda, three args.
  testCompile("((lambda (x y z) z) 5 6 7)", """
((let ((x0 (make-closure :f1 1))) (if (number? (closure-vars x0)) ((closure-proc x0) 5 6 7) ((closure-proc x0) (closure-vars x0) (new-tuple 5 6 7))))
(:f1 (x y z) z))""")

  // no frees in the lambda, four args.
  testCompile("((lambda (a b c d) a) 5 6 7 8)", """
((let ((x0 (make-closure :f1 1))) (if (number? (closure-vars x0)) ((closure-proc x0) (new-tuple 5 6 7 8)) ((closure-proc x0) (closure-vars x0) (new-tuple 5 6 7 8))))
(:f1 (args) (let ((a (aref args 0))) (let ((b (aref args 1))) (let ((c (aref args 2))) (let ((d (aref args 3))) a))))))""")

  // one free in the lambda, four args.
  testCompile("((lambda (a b c d) x) 5 6 7 8)", """
((let ((x0 (make-closure :f1 (new-tuple x)))) (if (number? (closure-vars x0)) ((closure-proc x0) (new-tuple 5 6 7 8)) ((closure-proc x0) (closure-vars x0) (new-tuple 5 6 7 8))))
(:f1 (frees args) (let ((a (aref args 0))) (let ((b (aref args 1))) (let ((c (aref args 2))) (let ((d (aref args 3))) (let ((x (aref frees 0))) x)))))))""")


  testCompile("((lambda (x) +) 6 7)", """
((let ((x0 (make-closure :f1 1))) (if (number? (closure-vars x0)) ((closure-proc x0) 6 7) ((closure-proc x0) (closure-vars x0) 6 7)))
(:f1 (x) (make-closure :f2 1))
(:f2 (x y) (+ x y)))
""")

  testCompile("(((lambda (x) print) 7) 7)", """
((let ((x0 (let ((x1 (make-closure :f2 1))) (if (number? (closure-vars x1)) ((closure-proc x1) 7) ((closure-proc x1) (closure-vars x1) 7))))) (if (number? (closure-vars x0)) ((closure-proc x0) 7) ((closure-proc x0) (closure-vars x0) 7)))
(:f2 (x) (make-closure :f3 1))
(:f3 (x) (print x)))
""")



  def testCompile(e:String, expected:String){
    test("compile: " + e){
      val compiler = new L5Compiler{}
      import compiler._
      val result = compileToString(e)
      verboseAssert(e, result, expected)
    }
  }
}

class L5FreeVarTests extends TestHelpers {

  testFreeVariables("(lambda (x) (x (y z)))", List("y", "z"))
  testFreeVariables("(lambda (x) (lambda (y) (y z)))", List("z"))
  testFreeVariables("(lambda () (x (y z)))", List("x", "y", "z"))
  testFreeVariables("x", List("x"))
  testFreeVariables("7", Nil)

  testFreeVariables("(let ((x 6)) x)", Nil)
  testFreeVariables("(let ((x y)) x)", List("y"))
  testFreeVariables("(let ((y 6)) x)", List("x"))
  testFreeVariables("(let ((y 6)) (x (y z)))", List("x", "z"))

  testFreeVariables("(let ((x x)) 7)", List("x"))
  testFreeVariables("(letrec ((x x)) 7)", Nil)
  testFreeVariables("(let ((x x)) x)", List("x"))
  testFreeVariables("(letrec ((x x)) x)", Nil)

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


  def testFreeVariables(e:String, expected:List[String]){
    test("free variables in: " + e){
      val compiler = new L5Compiler{}
      import compiler._
      println(parse(read(e)))
      expect(freeVars(parse(read(e))).mkString(" "))(expected.mkString(" "))
    }
  }
}
