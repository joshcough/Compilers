package L4Compiler

import util.{L3Interpreter, L4Interpreter, TestHelpers}

class L4CompilerTests extends TestHelpers{

  testCompile("(print 7)")
  testCompile("(print (new-tuple))")
  testCompile("(print (new-tuple 5 6 7))")
  testCompile("(print (new-tuple (+ 5 6) (+ 6 7) 7))")
  testCompile("(print (new-tuple (+ 5 6) 7 (+ 6 7)))")
  testCompile("(print (new-array 5 5))")
  testCompile("(print (let ((x (new-array 7 7))) x))")
  testCompile("(print (< 3 4))")
  testCompile("(print (< 4 3))")
  testCompile("(print (if (< 3 4) 5 6))")
  testCompile("(print (if (< 4 3) 5 6))")
  testCompile("(:f 5)", "(:f (x) (print x)))")
  testCompile("(print (+ (:f (+ 5 5)) (:f (+ 5 5))))", "(:f (x) (+ (+ x x) (+ x x))))")
  testCompile("(print (- (:f (+ 7 5)) (:f (* 2 (+ 1 0)))))", "(:f (x) (+ (+ x x) (= x x))))")
  testCompile("(print (let ((x (let ((x 5)) x))) x))")
  testCompile("(print (let ((x 4)) (let ((y x)) (let ((x 3)) (+ (:sq y) (:sq x))))))")
  testCompile("""
    (print
      (let ((x (make-closure :sq 7)))
        (let ((y (make-closure :double 7)))
          (+ ((closure-proc x) (closure-vars x)) ((closure-proc y) (closure-vars y)))
        )
      )
    )
    """)
  testCompile("""
    (print
      (let ((x (make-closure :sq 7)))
        (let ((y (make-closure :double 7)))
          (:sqdouble (+ ((closure-proc x) (closure-vars x)) ((closure-proc y) (closure-vars y))))
        )
      )
    )
  """)
  testCompile("""
    (begin
      (print
        (let ((x (make-closure :sq 7)))
          (let ((y (make-closure :double 7)))
            (:sqdouble (+ ((closure-proc x) (closure-vars x)) ((closure-proc y) (closure-vars y))))
          )
        )
      )
      (print (+ 5 6))
    )
  """)
  testCompile("(print (:nil))")
  testCompile("(print (:cons 1 (:cons 2 (:cons 3 (:nil)))))")
  testCompile("(print (:cons 1 (:cons 2 (:cons 3 (:cons 4 (:nil))))))")
  testCompile("(print (:cons 1 (:cons 2 (:cons 3 (:cons 4 (:cons 5 (:cons 6 (:cons 7 (:cons 8 (:nil))))))))))")
  testCompile("(print (:tolist (new-tuple 1 2 3 4)))")
  testCompile("(print (:map (:nil) :sq))")
  testCompile("(print (:map (:tolist (new-tuple 1 2 3 4)) :sq))")
  testCompile("(print (:filter (:tolist (new-tuple 1 2 3 4 5 6 7)) :greaterthan3))")
  testCompile("(print (:foldl :add 0 (:tolist (new-tuple 1 2 3 4 5 6))))")
  testCompile("(print (:foldl :mult 1 (:tolist (new-tuple 1 2 3 4 5 6))))")
  testCompile("(print (:some 7))")
  testCompile("(print (:none))")
  testCompile("(print (:find (:tolist (new-tuple 1 2 3 4 5 6 7)) :greaterthan3))")
  testCompile("(print (:find (:tolist (new-tuple 1 2 2 2 2 2 2)) :greaterthan3))")

  def program(e:String) = "(" + e + "\n" + functions + ")"

  def functions = """
  (:tolist (arr) (:tolisthelper arr 0))
  (:tolisthelper (arr index)
    (if
      (<= (alen arr) index)
      (:nil)
      (:cons (aref arr index) (:tolisthelper arr (+ 1 index)))
    )
  )
  (:map (list f)
    (if (:empty list)
      (:nil)
      (:cons (f (:head list)) (:map (:tail list) f))
    )
  )
  (:filter (list p)
    (if (:empty list)
      (new-tuple)
      (if (p (:head list))
        (:nil (:head list) (:filter (:tail list) p))
        (:cons (:tail list) p)
      )
    )
  )
  (:foldl (f init list)
    (if (:empty list)
      init
      (:foldl f (f init (:head list)) (:tail list))
    )
  )
  (:find (list p)
    (if (:empty list)
      (:none)
      (if (p (:head list))
        (:some (:head list))
        (:find (:tail list) p)
      )
    )
  )
  (:sq (x) (* x x))
  (:double (x) (+ x x))
  (:sqdouble (x) (* (:double x) (:double x)))
  (:greaterthan3 (x) (< 3 x))
  (:add (x y) (+ x y))
  (:mult (x y) (* x y))
  (:some (x) (new-tuple x))
  (:none () (new-tuple))
  (:nil () (new-tuple))
  (:cons (x list) (new-tuple x list))
  (:head (list) (aref list 0))
  (:tail (list) (aref list 1))
  (:empty (list) (= 0 (alen list)))
"""

  import io.FileHelper._
  new java.io.File("./4-test").mkdir()
  lazy val testcount = Iterator.from(0)

  def testCompile(L4E:String, extraFunctions: String = "") = {
    test(L4E){
      val L4Code = program(L4E + "\n" + extraFunctions)
      val compiler = new L4Compiler{}
      import compiler._
      val L3Code = compileToString(L4Code.clean)
//      println(L4Code.clean)
//      println("L3 Code: " + L3Code.clean)
//      verboseAssert(L4Code, read(L3Code.clean).toString, read(expected.clean).toString)
      val L4InterpResult = L4Interpreter.run(L4Code.clean)
      val L3InterpResult = L3Interpreter.run(L3Code.clean)
      println("L4InterpResult: "+ L4InterpResult)
      println("L3InterpResult: "+ L3InterpResult)

      if(L4InterpResult.trim == "") error("nothing happened in: " + L4Code)
      verboseAssert("L4 vs L3 interps", L4InterpResult, L3InterpResult)

      val index = testcount.next()
      // write the test
      new java.io.File("./4-test/test" + index + ".L4").write(L4Code.clean)
      // write the expected result
      new java.io.File("./4-test/test" + index + ".L3").write(L3Code.clean)
    }
  }
}

class L4CompileETests extends TestHelpers{

  compileETest("7", "7")
  compileETest("(b c)", "(b c)")
  compileETest("(a b c)", "(a b c)")
  compileETest("(a (b c))", "(let ((__x0 (b c))) (a __x0))")
  compileETest("(a (b (c d)))", "(let ((__x0 (c d))) (let ((__x1 (b __x0))) (a __x1)))")
  compileETest("(a (b (c d)) (x (y z)))",
    "(let ((__x0 (c d))) (let ((__x1 (b __x0))) (let ((__x2 (y z))) (let ((__x3 (x __x2))) (a __x1 __x3)))))")
  compileETest("(a (b (c d)) 7 (x (y z)))",
    "(let ((__x0 (c d))) (let ((__x1 (b __x0))) (let ((__x2 (y z))) (let ((__x3 (x __x2))) (a __x1 7 __x3)))))")
  compileETest("(let ((a 8)) 7)", "(let ((a 8)) 7)")
  compileETest("(let ((a (b c))) 7)", "(let ((a (b c))) 7)")
  compileETest("(let ((a (b (c d)))) 7)", "(let ((__x0 (c d))) (let ((a (b __x0))) 7))")
  compileETest("(print (a b))", "(let ((__x0 (a b))) (print __x0))")
  compileETest("(+ a b)", "(+ a b)")
  compileETest("(+ (a b) c)", "(let ((__x0 (a b))) (+ __x0 c))")
  compileETest("(+ c (a b))", "(let ((__x0 (a b))) (+ c __x0))")
  compileETest("(+ (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (+ __x0 __x1)))")
  compileETest("(- a b)", "(- a b)")
  compileETest("(- (a b) c)", "(let ((__x0 (a b))) (- __x0 c))")
  compileETest("(- c (a b))", "(let ((__x0 (a b))) (- c __x0))")
  compileETest("(- (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (- __x0 __x1)))")
  compileETest("(* a b)", "(* a b)")
  compileETest("(* (a b) c)", "(let ((__x0 (a b))) (* __x0 c))")
  compileETest("(* c (a b))", "(let ((__x0 (a b))) (* c __x0))")
  compileETest("(* (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (* __x0 __x1)))")
  compileETest("(< a b)", "(< a b)")
  compileETest("(< (a b) c)", "(let ((__x0 (a b))) (< __x0 c))")
  compileETest("(< c (a b))", "(let ((__x0 (a b))) (< c __x0))")
  compileETest("(< (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (< __x0 __x1)))")
  compileETest("(<= a b)", "(<= a b)")
  compileETest("(<= (a b) c)", "(let ((__x0 (a b))) (<= __x0 c))")
  compileETest("(<= c (a b))", "(let ((__x0 (a b))) (<= c __x0))")
  compileETest("(<= (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (<= __x0 __x1)))")
  compileETest("(= a b)", "(= a b)")
  compileETest("(= (a b) c)", "(let ((__x0 (a b))) (= __x0 c))")
  compileETest("(= c (a b))", "(let ((__x0 (a b))) (= c __x0))")
  compileETest("(= (a b) (c d))", "(let ((__x0 (a b))) (let ((__x1 (c d))) (= __x0 __x1)))")
  compileETest("((b c) (+ (- c d) (* 1 2)))",
    "(let ((__x0 (b c))) (let ((__x1 (- c d))) (let ((__x2 (* 1 2))) (let ((__x3 (+ __x1 __x2))) (__x0 __x3)))))")
  compileETest("(number? 7)", "(number? 7)")
  compileETest("(number? c)", "(number? c)")
  compileETest("(number? (b c))", "(let ((__x0 (b c))) (number? __x0))")
  compileETest("(a? 7)", "(a? 7)")
  compileETest("(a? c)", "(a? c)")
  compileETest("(a? (b c))", "(let ((__x0 (b c))) (a? __x0))")

  compileETest("(new-tuple a b c d e f g h i j k)", "(new-tuple a b c d e f g h i j k)")

  compileETest("(new-tuple a b c (+ d d) e f g h i j k)", "(let ((__x0 (+ d d))) (new-tuple a b c __x0 e f g h i j k))")
  compileETest("(new-tuple a b c d e f g h i j (+ k k))", "(let ((__x0 (+ k k))) (new-tuple a b c d e f g h i j __x0))")

  compileETest("(begin x y)", "(let ((__x0 x)) y)")
  compileETest("(begin (+ 1 2) y)", "(let ((__x0 (+ 1 2))) y)")
  compileETest("(begin y (+ 1 2))", "(let ((__x0 y)) (+ 1 2))")
  compileETest("(begin (+ 1 2) (+ 1 2))", "(let ((__x0 (+ 1 2))) (+ 1 2))")
  compileETest("(begin (a (b c)) 7)", "(let ((__x1 (b c))) (let ((__x0 (a __x1))) 7))")

  // these are just to show that what the begin code translates to is good.
  compileETest("(let ((__x0 (+ 1 2))) y)", "(let ((__x0 (+ 1 2))) y)")
  compileETest("(let ((__x0 y)) (+ 1 2))", "(let ((__x0 y)) (+ 1 2))")
  compileETest("(let ((__x0 (+ 1 2))) (+ 1 2))", "(let ((__x0 (+ 1 2))) (+ 1 2))")
  compileETest("(let ((__x1 (b c))) (let ((__x0 (a __x1))) 7))", "(let ((__x1 (b c))) (let ((__x0 (a __x1))) 7))")

  //(let ((passed (aref 0 results))) (new-tuple (if pass? (+ 1 passed) passed) (+ 1 (aref 1 results))))
  compileETest("""
(new-tuple
(let ((passed (aref 0 results)))
(if pass? (+ 1 passed) passed))
(+ 1 (aref 1 results)))""",
"""
(let ((passed (aref 0 results)))
(if pass?
(let ((__x0 (+ 1 passed)))
(let ((__x1 (aref 1 results)))
(let ((__x2 (+ 1 __x1)))
(new-tuple __x0 __x2))))
(let ((__x3 (aref 1 results)))
(let ((__x4 (+ 1 __x3)))
(new-tuple passed __x4)))))""")

  // this demonstrates the let renaming error from
  // http://www.eecs.northwestern.edu/~robby/courses/322-2011-spring/lecture09.pdf (pg 25)
  // c is free outside the let, but becomes captured by it!
  compileETest("((let ((c a)) (b c)) c)", "(let ((c a)) (let ((__x0 (b c))) (__x0 c)))", changeVarNames = false)
  // to avoid this, we rename all the variables first
  compileETest("((let ((d a)) (b d)) d)", "(let ((__x0 a)) (let ((__x1 (b __x0))) (__x1 d)))",
    changeVarNames = true)

  def compileETest(code:String, expected:String, changeVarNames:Boolean = false) = {
    test(code.clean){
      val res = compileE(code.clean, changeVarNames = changeVarNames)
      //println("compileETest(\"" + code + "\", " + "\"" + L4Printer.toCode(res) + "\")")
      verboseAssert(code, res, expected.clean.replace("\n", " "))
    }
  }

  def compileE(code:String, changeVarNames:Boolean = true, allowFrees:Boolean = false): String = {
    val compiler = new L4Compiler{}
    import compiler._
    val ast = parseE(read(code))
    L4Printer.toCode(if(changeVarNames) find(changeVarNamesInE(ast, allowFrees = true)) else find(ast))
  }
}


class ChangeVarNamesTests extends TestHelpers {

  // renames the let bound c only. the rest are free and this call allows frees.
  changeVarNamesInE("((let ((c a)) (b c)) c)", "((let ((__x0 a)) (b __x0)) c)", allowFrees = true)

  changeVarNamesInE("(let ((c 7)) (print c))", "(let ((__x0 7)) (print __x0))")

  changeVarNamesInE(
    "(let ((c 7)) (let ((c 7)) (print c)))",
    "(let ((__x0 7)) (let ((__x1 7)) (print __x1)))")

  changeVarNamesInE(
    "(let ((c 7)) (begin (let ((c 7)) (print c)) (print c)))",
    "(let ((__x0 7)) (begin (let ((__x1 7)) (print __x1)) (print __x0)))")

  changeVarNamesInE(
    "(let ((c 7)) (begin (let ((c c)) (print c)) (print c)))",
    "(let ((__x0 7)) (begin (let ((__x1 __x0)) (print __x1)) (print __x0)))")

  changeVarNamesInFunc(
    "(:f (x) (let ((c x)) (begin (let ((c x)) (print c)) (print c))))",
    "(:f (__x0) (let ((__x1 __x0)) (begin (let ((__x2 __x0)) (print __x2)) (print __x1))))")

  changeVarNamesInFunc(
    "(:f (x y z) (x y z))",
    "(:f (__x0 __x1 __x2) (__x0 __x1 __x2))")

  def changeVarNamesInE(code:String, expected:String, allowFrees:Boolean=false) = {
    test(code.clean){
      val compiler = new L4Compiler{}
      val res = compiler.changeVarNamesInE(compiler.parseE(compiler.read(code.clean)), allowFrees=allowFrees)
      verboseAssert(code, L4Printer.toCode(res), expected.clean.replace("\n", " "))
    }
  }

  def changeVarNamesInFunc(code:String, expected:String) = {
    test(code.clean){
      val compiler = new L4Compiler{}
      val res = compiler.changeVarNamesInFunc(compiler.parseFunction(compiler.read(code.clean)))
      verboseAssert(code, L4Printer.toCode(res), expected.clean.replace("\n", " "))
    }
  }
}
