package L2Compiler

import L2AST._
import java.io.File
import io.FileHelper._
import util.{L1Interpreter, L2Interpreter}
import io.{CommandRunner, Dir}

class L2CompilerHW extends L2CompilerTest with util.SlowTest {
  // all L1 files should also work in L2
  for(f<-Dir.L1TestFiles) { testCompileForHw(f.read) }

  // simple program
  testCompileForHw("""(((x <- 7) (eax <- (print x))))""")

  // interesting case. first, we rewrite to make edi, and esi into variables
  // and then the allocater realizes it can put put esi back into esi!
  // TODO: i wonder if i can detect that, and clobber that statement.
  // i should have a clobbering pass that kills instructions like that
  // and ones where the LHS doesnt appear in the out side. and maybe more.
  testCompileForHw("""((
        |(eax <- 7)
        |(ebx <- 7)
        |(ecx <- 7)
        |(edx <- 7)
        |(r:x <- eax)
        |(r:x += ebx)
        |(r:x += ecx)
        |(r:x += edx)
        |(r:x += eax)
        |(eax <- (print r:x))))""")

  // use lots of variables in main
  testCompileForHw("""((
        |(a <- 7)(b <- 9)(c <- 11)(d <- 13)
        |(e <- 15)(f <- 17)(g <- 19)(h <- 21)
        |(i <- 23)(j <- 25)(k <- 27)(l <- 29)
        |(eax <- (print a))(eax <- (print b))
        |(eax <- (print c))(eax <- (print d))
        |(eax <- (print e))(eax <- (print f))
        |(eax <- (print g))(eax <- (print h))
        |(eax <- (print i))(eax <- (print j))
        |(eax <- (print k))(eax <- (print l))))""")


  // use lots of variables in main and call a function
  testCompileForHw("""((
        |(a <- 7)(b <- 9)(c <- 11)(d <- 13)
        |(e <- 15)(f <- 17)(g <- 19)(h <- 21)
        |(i <- 23)(j <- 25)(k <- 27)(l <- 29)
        |(call :f)
        |(eax <- (print a))(eax <- (print b))
        |(eax <- (print c))(eax <- (print d))
        |(eax <- (print e))(eax <- (print f))
        |(eax <- (print g))(eax <- (print h))
        |(eax <- (print i))(eax <- (print j))
        |(eax <- (print k))(eax <- (print l)))
        |(:f
        |(a <- 29)(b <- 27)(c <- 25)(d <- 13)
        |(e <- 15)(f <- 17)(g <- 19)(h <- 21)
        |(i <- 23)(j <- 11)(k <- 9)(l <- 7)
        |(eax <- (print a))(eax <- (print b))
        |(eax <- (print c))(eax <- (print d))
        |(eax <- (print e))(eax <- (print f))
        |(eax <- (print g))(eax <- (print h))
        |(eax <- (print i))(eax <- (print j))
        |(eax <- (print k))(eax <- (print l))(return))
        |)""")

  // callee save registers are saved
  // random program...
  testCompileForHw("""((
        |(edi <- 7)
        |(esi <- 9)
        |(call :f)
        |(eax <- (print edi))
        |(eax <- (print esi)))
        |(:f
        |(a <- 29)(b <- 27)(c <- 25)(d <- 13)
        |(e <- 15)(f <- 17)(g <- 19)(h <- 21)
        |(i <- 23)(j <- 11)(k <- 9)(l <- 7)
        |(eax <- (print a))(eax <- (print b))
        |(eax <- (print c))(eax <- (print d))
        |(eax <- (print e))(eax <- (print f))
        |(eax <- (print g))(eax <- (print h))
        |(eax <- (print i))(eax <- (print j))
        |(eax <- (print k))(eax <- (print l))(return))
        )""")

  testCompileForHw("""
        |(((eax <- 1) (call :inc) (eax <- (print eax)))
        |(:inc
        |(eax += 1)
        |(cjump eax < 99 :continue :end)
        |:continue
        |(tail-call :inc)
        |:end
        |(return)))""")

  testCompileForHw("""
      |(((a <- 1)
      |(b <- 2)
      |(c <- 3)
      |(d <- 4)
      |(e <- 5)
      |(f <- 6)
      |(g <- 7)
      |(h <- 8)
      |(a <<= h)
      |(a += 1)
      |(eax <- (print a))
      |(a >>= g)
      |(a += 1)
      |(eax <- (print a))
      |(a <<= f)
      |(a += 1)
      |(eax <- (print a))
      |(a >>= e)
      |(a += 1)
      |(eax <- (print a))
      |(a <<= d)
      |(a += 1)
      |(eax <- (print a))
      |(a >>= c)
      |(a += 1)
      |(eax <- (print a))
      |(a <<= b)
      |(a -= 50)
      |(a >>= a)
      |(a += 1)
      |(eax <- (print a))))""")

  // interesting case here.... y isnt in the in or out set anywhere...
  // why? shouldnt it be in the out set of its own assignment statement? maybe not...
  // this could possibly be a case where we can whack that entire statement altogether,
  // because it didnt appear in the in or out set. JC 4/2010
  // -------------------------------------------------------
  // JC 4/18/2011 knows the answer. y isn't in its own out set because it is never used.
  // and yes, we can whack that assignment statement.
  // TODO: if we have an assignment statement and the variable doesn't appear in its
  // own out set, the whole statement is dead. this must apply for registers too.
  testCompile(
    input = "(((x <- 7)(y <- 8)(eax <- (print x))))",
    expected = Some("""
        |(((eax <- 7)
        |(ebx <- 8)
        |(eax <- (print eax))))"""))

  testCompile(
    input=
    """(((eax <- 7)
        |(ebx <- 7)
        |(ecx <- 7)
        |(edx <- 7)
        |(edi <- 7)
        |(esi <- 7)
        |(r:x <- eax)
        |(r:x += ebx)
        |(r:x += ecx)
        |(r:x += edx)
        |(r:x += edi)
        |(r:x += esi)
        |(r:x += eax)
        |(eax <- (print r:x))))""",
    error = Some("allocation impossible"))
}

//((x <- 1) (eax += x)) x -4 s
trait L2CompilerTest extends util.TestHelpers with L2Compiler {

  def End = None // sort of hacky, but whatever.
  def Just(i:Int) = Some(i)

  def testCompile(input:String, expected:Option[String] = None, error: Option[String] = None): Unit = {
    test(input.clean){
      if(expected == None && error == None) throw new IllegalStateException()
      try{
        val actual = toCode(compile(input.clean))
        if(! expected.isDefined) throw new IllegalStateException("expected error, but didnt get one!")
        verboseAssert(input, actual, expected.get)
      } catch{
        case e => error match {
          case Some(message) => assert(e.getMessage === message)
          case None => throw e
        }
      }
    }
  }

  CommandRunner.runAndDieOneErrors("rm -rf ./test/2-test")
  new java.io.File("./test/2-test").mkdir()
  val count = Iterator.from(0)

  def testCompileForHw(L2Code:String) = {
    val index = count.next()
    test(index + " - " + L2Code.clean){
      val L1Code = toCode(compile(L2Code.clean))
      val L1InterpResult =  L1Interpreter.run(L1Code.clean)
      val L2InterpResult = L2Interpreter.run(L2Code.clean)
      // write out the tests files and results.
      // write the test
      new File("./test/2-test/test" + index + ".L2").write(L2Code.clean)
      // write the expected result
      new File("./test/2-test/test" + index + ".L1").write(L1Code.clean)
      verboseAssert(L2Code, L1InterpResult, L2InterpResult)
    }
  }
}

// TODO: might want to use this in a test here somewhere:
//    println("spill var: " + chooseSpillVar(liveRanges(inoutForTesting(code))))
//    val spillVar = chooseSpillVar(liveRanges(inoutForTesting(code))).get
//    val newProgram = spill(spillVar, -4, parseListOfInstructions(code))
//    println("code after spilling: " + newProgram.map(L2Printer.toCode).mkString("(", "\n", ")"))
