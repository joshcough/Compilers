package LCompiler

import util.{TestHelpers, Timer}
import util.{L2Interpreter, L3Interpreter, L4Interpreter, L1Interpreter, L5Interpreter, TestHelpers}
import collection.mutable.ListBuffer

class LCompilerTests extends TestHelpers with Timer {

  val compilerTests = new ListBuffer[String]

  registerTest("(test 5 5)")
//  registerTest("(test (identity 14) 14)")
//  registerTest("(test false false)")
//  registerTest("(test true true)")
//  registerTest("(test true (not false))")
//  registerTest("(test false (not true))")
//  registerTest("(test (and true true) true)")
//  registerTest("(test (and true false) false)")
//  registerTest("(test (and false true) false)")
//  registerTest("(test (and false false) false)")
//  registerTest("(test (or true true) true)")
//  registerTest("(test (or true false) true)")
//  registerTest("(test (or false true) true)")
//  registerTest("(test (or false false) false)")
//  registerTest("(test (+ 5 5) 10)")
//  registerTest("(test (((lambda () +)) 7 7) 14)")
//  registerTest("(test (empty nil) true)")
//  registerTest("(test (size nil) 0)")
//  registerTest("(test (size (cons 1 nil)) 1)")
//  registerTest("(test (size (cons 7 (cons 1 nil))) 2)")
//  registerTest("(test (size (cons 5 (cons 9 (cons 14 (cons 1 nil))))) 4)")
//  registerTest("(test (tolist (new-tuple)) nil)")
//  registerTest("(test (tolist (new-tuple 1)) (cons 1 nil))")
//  registerTest("(test (tolist (new-tuple 1 2 3 4)) (cons 1 (cons 2 (cons 3 (cons 4 nil)))))")
//  registerTest("(test (map identity nil) nil)")
//  registerTest("(test (map (lambda (x) (+ x 5)) nil) nil)")
//  registerTest("(test (map identity (tolist (new-tuple 1 2 3 4))) (tolist (new-tuple 1 2 3 4)))")
//  registerTest("(test (map (lambda (x) (+ x 5)) (tolist (new-tuple 1 2 3 4))) (tolist (new-tuple 6 7 8 9)))")
//  registerTest("(test (reduce + 0 (tolist (new-tuple 1 2 3 4))) 10)")
//  registerTest("(test (reduce * 1 (tolist (new-tuple 1 2 3 4))) 24)")
//  registerTest("(test (mapreduce (lambda (x) (* x x)) + 0 (tolist (new-tuple 1 2 3 4))) 30)")
//  registerTest("(test (filter (lambda (x) (< 2 x)) (tolist (new-tuple 1 2 3 4))) (tolist (new-tuple 3 4)))")
//  registerTest("(test (find (lambda (x) (= 4 x)) (tolist (new-tuple 1 2 3 4))) (some 4))")
//  registerTest("(test (get (find (lambda (x) (= 4 x)) (tolist (new-tuple 1 2 3 4)))) 4)")
//  registerTest("(test (find (lambda (x) (= 5 x)) (tolist (new-tuple 1 2 3 4))) none)")
//  registerTest("(test (stake 1 (ones)) (tolist (new-tuple 1)))")
//  registerTest("(test (stake 5 (ones)) (tolist (new-tuple 1 1 1 1 1)))")
//  registerTest("(test (stake 5 (nats)) (tolist (new-tuple 1 2 3 4 5)))")
//  registerTest("(test (stake 5 (smap (lambda (x) (+ 7 x)) (ones))) (tolist (new-tuple 8 8 8 8 8)))")
//  registerTest("(test (stake 5 (szipwith + (nats) (nats))) (tolist (new-tuple 2 4 6 8 10)))")
//  registerTest("(test (stake 10 (fibs)) (tolist (new-tuple 0 1 1 2 3 5 8 13 21 34)))")

  //for(t<-compilerTests){ testCompile(t) }
  //for(t<-compilerTests){ testL4Compiler(t) }
  //for(t<-compilerTests){ testL2Compiler(t) }
  for(t<-compilerTests){ testL1Compiler(t) }

  def registerTest(code:String){ compilerTests += code }


  def wrapLibrary(code:String) = """
  (let ([identity (lambda (x) x)])
  (let ([constant (lambda (x) (lambda () x))])

  ;;;;;;;;;;;;;;;;;;
  ;; Booleans
  ;;;;;;;;;;;;;;;;;;
  (let ([true 1])
  (let ([false 0])
  (let ([not (lambda (x) (if (= true x) false true))])
  (let ([and (lambda (x y) (if (= false x) false (if (= false y) false true)))])
  (let ([or  (lambda (x y) (if (= false x) (if (= false y) false true) true))])

  ;;;;;;;;;;;;;;;;;;
  ;; Options
  ;;;;;;;;;;;;;;;;;;
  (let ([none (new-tuple)])
  (let ([some (lambda (x) (new-tuple x (new-tuple)))])
  (let ([get (lambda (o) (aref o 0))])

  ;;;;;;;;;;;;;;;;;;
  ;; Lists
  ;;;;;;;;;;;;;;;;;;
  (let ([nil (new-tuple)])
  (let ([cons (lambda (x y) (new-tuple x y))])
  (let ([head (lambda (xs) (aref xs 0))])
  (let ([tail (lambda (xs) (aref xs 1))])
  (let ([empty (lambda (xs) (= 0 (alen xs)))])
  (letrec ([size (lambda (xs) (if (empty xs) 0 (+ 1 (size (tail xs)))))])
  (letrec ([mapreduce (lambda (f g id xs)
    (if (empty xs) id (g (f (head xs)) (mapreduce f g id (tail xs)))))])
  (let ([map (lambda (f xs) (mapreduce f cons nil xs))])
  (let ([reduce (lambda (f id xs) (mapreduce identity f id xs))])
  (let ([tolist (lambda (arr)
    (letrec ((helper (lambda (index)
      (if (<= (alen arr) index) nil (cons (aref arr index) (helper (+ 1 index)))))))
    (helper 0))
  )])
  (let ([filter (lambda (p xs) (mapreduce identity (lambda (x y) (if (p x) (cons x y) y)) nil xs))])
  (letrec ([find (lambda (p xs)
    (if (empty xs) none (if (p (head xs)) (some (head xs)) (find p (tail xs))))
  )])

  ;;;;;;;;;;;;;;;;;;
  ;; Streams
  ;;;;;;;;;;;;;;;;;;
  (let ([scons (lambda (x y) (cons x y))])
  (let ([shead (lambda (s) (head s))])
  (let ([stail (lambda (s) ((tail s)))])

  (letrec ([stake (lambda (n s) (if (= n 0) nil (cons (shead s) (stake (- n 1) (stail s)))))])
  (letrec ([smap (lambda (f s) (scons (f (shead s)) (lambda () (smap f (stail s)))))])
  (letrec ([szip (lambda (s1 s2)
    (scons (cons (shead s1) (shead s2)) (lambda () (szip (stail s1) (stail s2))))
  )])
  (letrec ([szipwith (lambda (f s1 s2)
    (scons (f (shead s1) (shead s2)) (lambda () (szipwith f (stail s1) (stail s2))))
  )])

  (letrec ([ones (lambda () (scons 1 ones))])
  (letrec ([nats (lambda () (scons 1 (lambda () (smap (lambda (x) (+ 1 x)) (nats)))))])
  (letrec ([fibs (lambda ()
    (scons 0 (lambda () (scons 1 (lambda () (szipwith + (fibs) (stail (fibs)))))))
  )])

  ;;;;;;;;;;;;;;;;;;
  ;; Equality
  ;;;;;;;;;;;;;;;;;;
  (letrec ([eqlist (lambda (x y)
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
  (let ([eq (lambda (x y) (if (and (number? x) (number? y)) (= x y) (eqlist x y)))])

  ;;;;;;;;;;;;;;;;;;
  ;; test function
  ;;;;;;;;;;;;;;;;;;
  (let ([test (lambda (x y) (if (eq x y) (print 1) (begin (print x) (print y))))])
""" + code + """
  )))))))))))))))))))))))))))))))))))
"""


  import io.FileHelper._
  new java.io.File("./test/the-test").mkdir()
  lazy val testcount = Iterator.from(0)

  def testCompile(L5E:String) = {
    val index = testcount.next()
    test(index + "-" + L5E){
      val l5 = wrapLibrary(L5E.clean)
      //val l5 = L5E.clean
      val all@List(_, l4, l3, l2, l1, x86) = alwaysTimed("compiling", LCompiler.compileToStrings(l5))

      // write the results
      write(all)

      def die(level:String, otherResult:String, codes:String*){
        println(codes.mkString("\n============\n"))
        fail(level + "\nother result: " + otherResult)
      }

      val L5InterpResult = L5Interpreter.run(l5)
      val L1InterpResult = L1Interpreter.run(l1)

      verboseAssert(L5E, L5InterpResult, "1")
      if(L5InterpResult != L1InterpResult) {
        val L4InterpResult = L4Interpreter.run(l4)
        if(L5InterpResult == L4InterpResult) {
          val L3InterpResult = L3Interpreter.run(l3)
          if(L5InterpResult == L3InterpResult) {
            val L2InterpResult = L2Interpreter.run(l2)
            if(L5InterpResult == L2InterpResult) {
              die("L5 != L1", L1InterpResult, l5, l4, l3, l2, l1)
            }
            else die("L5 != L2", L2InterpResult, l5, l4, l3, l2)
          }
          else die("L5 != L3", L3InterpResult, l5, l4, l3)
        }
        else die("L5 != L4", L4InterpResult, l5, l4)
      }
    }
  }

  def testL4Compiler(L5E:String){
    test(testcount.next() + "-" + L5E){
      write(LCompiler.compileToStrings(wrapLibrary(L5E.clean), compileTo = 3))
    }
  }

  def testL2Compiler(L5E:String){
    test(testcount.next() + "-" + L5E){
      write(LCompiler.compileToStrings(wrapLibrary(L5E.clean), compileTo = 1))
    }
  }

  def testL1Compiler(L5E:String){
    test(testcount.next() + "-" + L5E){
      write(LCompiler.compileToStrings(wrapLibrary(L5E.clean), compileTo = 0))
    }
  }

  def write(strings:List[String], count:Int=0) {
    for((s, i) <- strings.zipWithIndex){
      println(s)
      new java.io.File("./test/the-test/test" + count + (".L" + (5-i))).write(s)
    }
  }
}
