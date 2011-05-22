package L5Compiler

import util.{L5Interpreter, L4Interpreter, TestHelpers}
import L5Compiler.L5AST.{E, Num, Variable}

// hi.
class L5CompilerTests extends TestHelpers {

  testCompile("(test 5 5)")
  testCompile("(test (identity 14) 14)")
  testCompile("(test false false)")
  testCompile("(test true true)")
  testCompile("(test true (not false))")
  testCompile("(test false (not true))")
  testCompile("(test (and true true) true)")
  testCompile("(test (and true false) false)")
  testCompile("(test (and false true) false)")
  testCompile("(test (and false false) false)")
  testCompile("(test (or true true) true)")
  testCompile("(test (or true false) true)")
  testCompile("(test (or false true) true)")
  testCompile("(test (or false false) false)")
  testCompile("(test (+ 5 5) 10)")
  testCompile("(test (((lambda () +)) 7 7) 14)")
  testCompile("(test (empty nil) true)")
  testCompile("(test (size nil) 0)")
  testCompile("(test (size (cons 1 nil)) 1)")
  testCompile("(test (size (cons 7 (cons 1 nil))) 2)")
  testCompile("(test (size (cons 5 (cons 9 (cons 14 (cons 1 nil))))) 4)")
  testCompile("(test (tolist (new-tuple)) nil)")
  testCompile("(test (tolist (new-tuple 1)) (cons 1 nil))")
  testCompile("(test (tolist (new-tuple 1 2 3 4)) (cons 1 (cons 2 (cons 3 (cons 4 nil)))))")
  testCompile("(test (map identity nil) nil)")
  testCompile("(test (map (? (x) (+ x 5)) nil) nil)")
  testCompile("(test (map identity (tolist (new-tuple 1 2 3 4))) (tolist (new-tuple 1 2 3 4)))")
  testCompile("(test (map (? (x) (+ x 5)) (tolist (new-tuple 1 2 3 4))) (tolist (new-tuple 6 7 8 9)))")
  testCompile("(test (reduce + 0 (tolist (new-tuple 1 2 3 4))) 10)")
  testCompile("(test (reduce * 1 (tolist (new-tuple 1 2 3 4))) 24)")
  testCompile("(test (mapreduce (? (x) (* x x)) + 0 (tolist (new-tuple 1 2 3 4))) 30)")
  testCompile("(test (filter (? (x) (< 2 x)) (tolist (new-tuple 1 2 3 4))) (tolist (new-tuple 3 4)))")
  testCompile("(test (find (? (x) (= 4 x)) (tolist (new-tuple 1 2 3 4))) (some 4))")
  testCompile("(test (get (find (? (x) (= 4 x)) (tolist (new-tuple 1 2 3 4)))) 4)")
  testCompile("(test (find (? (x) (= 5 x)) (tolist (new-tuple 1 2 3 4))) none)")


  def wrapLibrary(code:String) = """
  (let ([identity (? (x) x)])

  ;;;;;;;;;;;;;;;;;;
  ;; Booleans
  ;;;;;;;;;;;;;;;;;;
  (let ([true 1])
  (let ([false 0])
  (let ([not (? (x) (if (= true x) false true))])
  (let ([and (? (x y) (if (= false x) false (if (= false y) false true)))])
  (let ([or  (? (x y) (if (= false x) (if (= false y) false true) true))])

  ;;;;;;;;;;;;;;;;;;
  ;; Options
  ;;;;;;;;;;;;;;;;;;
  (let ([none (new-tuple)])
  (let ([some (? (x) (new-tuple x (new-tuple)))])
  (let ([get (? (o) (aref o 0))])

  ;;;;;;;;;;;;;;;;;;
  ;; Lists
  ;;;;;;;;;;;;;;;;;;
  (let ([nil (new-tuple)])
  (let ([cons (? (x y) (new-tuple x y))])
  (let ([head (? (xs) (aref xs 0))])
  (let ([tail (? (xs) (aref xs 1))])
  (let ([empty (? (xs) (= 0 (alen xs)))])
  (letrec ([size (? (xs) (if (empty xs) 0 (+ 1 (size (tail xs)))))])
  (letrec ([mapreduce (? (f g id xs)
    (if (empty xs) id (g (f (head xs)) (mapreduce f g id (tail xs)))))])
  (let ([map (? (f xs) (mapreduce f cons nil xs))])
  (let ([reduce (? (f id xs) (mapreduce identity f id xs))])
  (let ([tolist (? (arr)
    (letrec ((helper (? (index)
      (if (<= (alen arr) index) nil (cons (aref arr index) (helper (+ 1 index)))))))
    (helper 0))
  )])
  (let ([filter (? (p xs) (mapreduce identity (? (x y) (if (p x) (cons x y) y)) nil xs))])
  (letrec ([find (? (p xs)
    (if (empty xs) none (if (p (head xs)) (some (head xs)) (find p (tail xs))))
  )])

  ;;;;;;;;;;;;;;;;;;
  ;; Equality
  ;;;;;;;;;;;;;;;;;;
  (letrec ([eqlist (? (x y)
    (if (and (empty x) (empty y))
      true
      (if (or (empty x) (empty y))
        false
        (and
          (if (and (number? (head x)) (number? (head y)))
            (= (head x) (head y))
            (if (and (a? (head x)) (a? (head y)))
              (eqlist (head x) (head y))
              false
            )
          )
          (eqlist (tail x) (tail y))
        )
      )
    ))])
  (let ([eq (? (x y) (if (and (number? x) (number? y)) (= x y) (eqlist x y)))])

  ;;;;;;;;;;;;;;;;;;
  ;; test function
  ;;;;;;;;;;;;;;;;;;
  (let ([test (?(x y) (if (eq x y) (print 1) (begin (print x) (print y))))])
""" + code + """
  ))))))))))))))))))))))))
"""

  import io.FileHelper._
  new java.io.File("./test/5-test").mkdir()
  lazy val testcount = Iterator.from(0)

  def testCompile(L5E:String) = {
    val index = testcount.next()
    test(index + "-" + L5E){
      val L5Code = wrapLibrary(L5E)
      val compiler = new L5Compiler{}
      import compiler._
      val L5Output = toCode(parseE(read(L5Code.clean))).clean
      val L4Code = compileToString(L5Code.clean)

      // write the test
      new java.io.File("./test/5-test/test" + index + ".L5").write(L5Output)
      // write the expected result
      new java.io.File("./test/5-test/test" + index + ".L4").write(L4Code.clean)

      val L5InterpResult = L5Interpreter.run(L5Output)
      val L4InterpResult = L4Interpreter.run(L4Code.clean)
      if(L5InterpResult.trim == "") error("nothing happened in: " + L5Output)
      verboseAssert("L5 vs L4 interps", L5InterpResult, L4InterpResult)
      verboseAssert(L5E, L5InterpResult, "1")
    }
  }

}

class L5CompileETests extends TestHelpers {

  testCompile("7", "(7)")

  // one arg, no frees
  testCompile("(lambda (x) x)", """
(:f0
(:f0 (x) x))""")

  // one arg, one free
  testCompile("(lambda (x) y)", """
((make-closure :f0 (new-tuple y))
(:f0 (frees x) (let ([y (aref frees 0)]) y)))
""")

  // two args, one free
  // (we need frees, because there are free vars, but we can fit the rest in as args)
  testCompile("(lambda (x y) z)", """
((make-closure :f0 (new-tuple z))
(:f0 (frees x y) (let ([z (aref frees 0)]) z)))
""")

  // three args, none free (dont need frees or args!)
  testCompile("(lambda (a b c) a)", """
(:f0
(:f0 (a b c) a))
""")

  // three args, one free (we need both frees and args here.)
  testCompile("(lambda (a b c) z)", """
((make-closure :f0 (new-tuple z))
(:f0 (frees args) (let ([a (aref args 0)]) (let ([b (aref args 1)]) (let ([c (aref args 2)]) (let ([z (aref frees 0)]) z))))))
""")

  // four args, none free (over the limit, but notice we dont have 'frees', just 'args')
  testCompile("(lambda (a b c d) a)", """
(:f0
(:f0 (args) (let ([a (aref args 0)]) (let ([b (aref args 1)]) (let ([c (aref args 2)]) (let ([d (aref args 3)]) a))))))
""")

  // inner lambda
  testCompile("(lambda (x) (lambda (y) y))", """
(:f1
(:f1 (x) :f0)
(:f0 (y) y))
""")

  testCompile("(+ 5 6)", "((+ 5 6))")

  testCompile("(+ 5 (lambda (x) x))", """
((+ 5 :f0)
(:f0 (x) x))""")

  // no frees in the lambda. one arg.
  testCompile("((lambda (x) x) 5)", """
((let ([x0 :f0]) (if (a? x0) ((closure-proc x0) (closure-vars x0) 5) (x0 5)))
(:f0 (x) x))""")

  // no frees in the lambda. two args.
  testCompile("((lambda (x y) x) 5 6)", """
((let ([x0 :f0]) (if (a? x0) ((closure-proc x0) (closure-vars x0) 5 6) (x0 5 6)))
(:f0 (x y) x))""")

  // no frees in the lambda, three args.
  testCompile("((lambda (x y z) z) 5 6 7)", """
((let ([x0 :f0]) (if (a? x0) ((closure-proc x0) (closure-vars x0) (new-tuple 5 6 7)) (x0 5 6 7)))
(:f0 (x y z) z))""")

  // no frees in the lambda, four args.
  testCompile("((lambda (a b c d) a) 5 6 7 8)", """
((let ([x0 :f0]) (if (a? x0) ((closure-proc x0) (closure-vars x0) (new-tuple 5 6 7 8)) (x0 (new-tuple 5 6 7 8))))
(:f0 (args) (let ([a (aref args 0)]) (let ([b (aref args 1)]) (let ([c (aref args 2)]) (let ([d (aref args 3)]) a))))))""")

  // one free in the lambda, four args.
  testCompile("((lambda (a b c d) x) 5 6 7 8)", """
((let ([x0 (make-closure :f0 (new-tuple x))]) (if (a? x0) ((closure-proc x0) (closure-vars x0) (new-tuple 5 6 7 8)) (x0 (new-tuple 5 6 7 8))))
(:f0 (frees args) (let ([a (aref args 0)]) (let ([b (aref args 1)]) (let ([c (aref args 2)]) (let ([d (aref args 3)]) (let ([x (aref frees 0)]) x)))))))""")


  testCompile("((lambda (x) +) 6 7)", """
((let ([x0 :f1]) (if (a? x0) ((closure-proc x0) (closure-vars x0) 6 7) (x0 6 7)))
(:f1 (x) :f0)
(:f0 (x y) (+ x y)))
""")

  testCompile("(((lambda (x) print) 7) 7)", """
((let ([x0 (let ([x1 :f1]) (if (a? x1) ((closure-proc x1) (closure-vars x1) 7) (x1 7)))]) (if (a? x0) ((closure-proc x0) (closure-vars x0) 7) (x0 7)))
(:f1 (x) :f0)
(:f0 (x) (print x)))
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


class L5SubTests extends TestHelpers {

  testSub("(lambda (x) (x (y z)))", Variable("z"), Num(0), "(lambda (x) (x (y 0)))")

  def testSub(e1:String, v:Variable, e2:E, expected:String){
    test("Sub: " + e1){
      val compiler = new L5Compiler{}
      import compiler._
      verboseAssert(e1, compiler.toCode(compiler.sub(v, parse(read(e1)), e2)), expected)
    }
  }
}


class L5FreeVarTests extends TestHelpers {

  testFreeVariables("(lambda (x) (x (y z)))", List("y", "z"))
  testFreeVariables("(lambda (x) (lambda (y) (y z)))", List("z"))
  testFreeVariables("(lambda () (x (y z)))", List("x", "y", "z"))
  testFreeVariables("x", List("x"))
  testFreeVariables("7", Nil)

  testFreeVariables("(let ([x 6)) x)", Nil)
  testFreeVariables("(let ([x y)) x)", List("y"))
  testFreeVariables("(let ([y 6)) x)", List("x"))
  testFreeVariables("(let ([y 6)) (x (y z)))", List("x", "z"))

  testFreeVariables("(let ([x x)) 7)", List("x"))
  testFreeVariables("(letrec ((x x)) 7)", Nil)
  testFreeVariables("(let ([x x)) x)", List("x"))
  testFreeVariables("(letrec ((x x)) x)", Nil)

  testFreeVariables("(if 1 7 8)", Nil)
  testFreeVariables("(if 1 7 z)", List("z"))
  testFreeVariables("(if 1 y 8)", List("y"))
  testFreeVariables("(if x y z)", List("x", "y", "z"))
  testFreeVariables("(let ([y 6)) (if 1 y 7))", Nil)
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