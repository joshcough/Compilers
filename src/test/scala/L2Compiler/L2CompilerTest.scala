package L2Compiler

import L2AST._
import java.io.File

class L2CompilerTests extends L2CompilerTest {

  testCompile(
    input =
    """(((x <- 7)
        |(eax <- (print x))))""",
  
    expected=Some(
    """(((eax <- 7)
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

  // interesting case. first, we rewrite to make edi, and esi into variables
  // and then the allocater realizes it can put put esi back into esi!
  // TODO: i wonder if i can detect that, and clobber that statement.
  // i should have a clobbering pass that kills instructions like that
  // and ones where the LHS doesnt appear in the out side. and maybe more.
  testCompile(
    input=
    """(((eax <- 7)
        |(ebx <- 7)
        |(ecx <- 7)
        |(edx <- 7)
        |(r:x <- eax)
        |(r:x += ebx)
        |(r:x += ecx)
        |(r:x += edx)
        |(r:x += eax)))""",
    expected = Some(
    """(((eax <- 7)
        |(ebx <- 7)
        |(ecx <- 7)
        |(edx <- 7)
        |(edi <- eax)
        |(edi += ebx)
        |(edi += ecx)
        |(edi += edx)
        |(edi += eax)))"""))

  testCompile(
    """
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
      |(eax <- (print a))))""",
    expected = Some(
    """?"""))


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

}

//((x <- 1) (eax += x)) x -4 s
trait L2CompilerTest extends util.TestHelpers with L2CompilerExtras {

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
}

// TODO: might want to use this in a test here somewhere:
//    println("spill var: " + chooseSpillVar(liveRanges(inoutForTesting(code))))
//    val spillVar = chooseSpillVar(liveRanges(inoutForTesting(code))).get
//    val newProgram = spill(spillVar, -4, parseListOfInstructions(code))
//    println("code after spilling: " + newProgram.map(L2Printer.toCode).mkString("(", "\n", ")"))
