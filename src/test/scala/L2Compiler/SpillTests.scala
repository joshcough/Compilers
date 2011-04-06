package L2Compiler

import L2AST._
import collection.mutable.ListBuffer

/**
 * (((x <- 1) (eax += x))) x -4 s
 *   (((mem ebp -4) <- 1)
 *  (s_0 <- (mem ebp -4))
 *  (eax += s_0))
 */
class SpillTests extends L2CompilerTest {

  val count = Iterator.from(0)

  testSpill("((x <- 1) (eax += x))", "(((mem ebp -4) <- 1) (s_0 <- (mem ebp -4)) (eax += s_0))")
  testSpill("((x <- 7))", "(((mem ebp -4) <- 7))")
  testSpill("((x <- eax))", "(((mem ebp -4) <- eax))")
  testSpill("((x <- (mem y 4)))", "((s_0 <- (mem y 4)) ((mem ebp -4) <- s_0))")
  testSpill("((x <- (mem x 4)))", "((s_0 <- (mem ebp -4)) (s_1 <- (mem s_0 4)) ((mem ebp -4) <- s_1))")
  testSpill("((y <- (mem x 4)))", "((s_0 <- (mem ebp -4)) (y <- (mem s_0 4)))")
  testSpill("((x <- x < x))", "((s_0 <- (mem ebp -4)) (s_1 <- s_0 < s_0) ((mem ebp -4) <- s_1))")
  testSpill("((x <- x < y))", "((s_0 <- (mem ebp -4)) (s_1 <- s_0 < y) ((mem ebp -4) <- s_1))")
  testSpill("((x <- y < x))", "((s_0 <- (mem ebp -4)) (s_1 <- y < s_0) ((mem ebp -4) <- s_1))")
  testSpill("((x <- y < z))", "((s_0 <- y < z) ((mem ebp -4) <- s_1))")
  testSpill("((y <- x < x))", "((s_0 <- (mem ebp -4)) (y <- s_0 < s_0))")
  testSpill("((y <- x < z))", "((s_0 <- (mem ebp -4)) (y <- s_0 < z))")
  testSpill("((y <- z < x))", "((s_0 <- (mem ebp -4)) (y <- z < s_0))")
  testSpill("((y <- z1 < z2))", "((y <- z1 < z2))")
  testSpill("((x <- eax < ebx)))", "((s_0 <- eax < ebx) ((mem ebp -4) <- s_1))")
  testSpill("((eax += x))", "((s_0 <- (mem ebp -4)) (eax += s_0))")
  testSpill("((x += eax))", "((s_0 <- (mem ebp -4)) (s_0 += eax) ((mem ebp -4) <- s_0))")
  testSpill("((x += y))", "((s_0 <- (mem ebp -4)) (s_0 += y) ((mem ebp -4) <- s_0))")
  testSpill("((eax <- (print x)))", "((s_0 <- (mem ebp -4)) (eax <- (print s_0)))")
  testSpill("((goto x))", "((s_0 <- (mem ebp -4)) (goto s_0))")
  testSpill("((call x))", "((s_0 <- (mem ebp -4)) (call s_0))")
  testSpill("(((mem x 4) <- x))", "((s_0 <- (mem ebp -4)) ((mem s_0 4) <- s_0))")
  testSpill("(((mem x 4) <- y))", "((s_0 <- (mem ebp -4)) ((mem s_0 4) <- y))")
  testSpill("(((mem y 4) <- x))", "((s_0 <- (mem ebp -4)) ((mem s_0 4) <- x))")
  testSpill("(((mem y 4) <- z))", "(((mem y 4) <- z))")
  testSpill("((eax <- (allocate x x)))", "((s_0 <- (mem ebp -4)) (eax <- (allocate s_0 s_0)))")
  testSpill("((eax <- (allocate x y)))", "((s_0 <- (mem ebp -4)) (eax <- (allocate s_0 y)))")
  testSpill("((eax <- (allocate y x)))", "((s_0 <- (mem ebp -4)) (eax <- (allocate y s_0)))")
  testSpill("((eax <- (allocate y y)))", "((eax <- (allocate y y)))")
  testSpill("((eax <- (allocate x x))(eax <- (allocate x x)))", "((s_0 <- (mem ebp -4)) (eax <- (allocate s_0 s_0)) (s_1 <- (mem ebp -4)) (eax <- (allocate s_1 s_1)))")
  testSpill("((cjump x < x :l1 :l2))", "((s_0 <- (mem ebp -4)) (cjump s_0 < s_0 :l1 :l2))")
  testSpill("((cjump x < y :l1 :l2))", "((s_0 <- (mem ebp -4)) (cjump s_0 < y :l1 :l2))")
  testSpill("((cjump y < x :l1 :l2))", "((s_0 <- (mem ebp -4)) (cjump y < s_0 :l1 :l2))")
  testSpill("((cjump y < y :l1 :l2))", "((cjump y < y :l1 :l2))")

  def testSpill(code:String, expected: String) = {
    def doSpill(code: String) = {
      val newProgramList =
        compiler.spill(Variable("x"), -4, "s_", compiler.parseInstructionListThing(code))
      newProgramList.map(L2Printer.toCode).mkString("(", " ", ")")
    }

    test(code){
      val actual = doSpill(code)
      assert(actual === expected)
      println("hi")
      // write out the tests files and results.
      import java.io.File
      import L1Compiler.FileHelper._
      val index = count.next()
      val f = new File("./test/test" + index + ".L2f")
      // write the test
      f.write(code + " x -4 s_")
      // write the expected result
      val spillResFile = new File("./test/test" + index + ".sres")
      spillResFile.write(doSpill(code))
    }
  }
}

