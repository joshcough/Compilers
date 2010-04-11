package L1Compiler

import reader._

abstract class L1ParserTest extends org.scalatest.FunSuite{
  val parser = L1Parser

  def read(s:String): Any = new Reader().read(s)
  def parse(a:Any) = parser parse a

  def testParse(t: (String, Exp)): Unit = {
    test(t._1 + " => " + t._2){
      assert(parse(read(t._1)) === t._2)
    }
  }
}

class ParsePrimitivesTest extends L1ParserTest {
  testParse(""" eax """  -> eax)
  testParse(""" ebx """  -> ebx)
  testParse(""" ecx """  -> ecx)
  testParse(""" edx """  -> edx)
  testParse(""" esi """  -> esi)
  testParse(""" edi """  -> edi)
  testParse(""" ebp """  -> ebp)
  testParse(""" esp """  -> esp)
  testParse(""" :hey """  -> Label("hey"))
  testParse(""" 1 """  -> Num(1))
}

class ParseAssignmentsTest extends L1ParserTest {

  // simple register writes
  testParse(""" (eax <- 7) """  -> RegisterAssignment(eax, Num(7)))
  testParse(""" (eax <- ebx) """  -> RegisterAssignment(eax, ebx))
  testParse(""" (ebx <- eax) """  -> RegisterAssignment(ebx, eax))

  // mem read
  testParse(""" (eax <- (mem eax 4)) """  ->
          RegisterAssignment(eax, MemRead(MemLoc(eax, Num(4)))))
  testParse(""" (eax <- (mem ebx 4)) """  ->
          RegisterAssignment(eax, MemRead(MemLoc(ebx, Num(4)))))

  // mem write
  testParse(""" ((mem ebx 4) <- 7) """  -> MemWrite(MemLoc(ebx, Num(4)), Num(7)))
  testParse(""" ((mem ebx 4) <- eax) """  -> MemWrite(MemLoc(ebx, Num(4)), eax))
  testParse(""" ((mem ebx 4) <- esi) """  -> MemWrite(MemLoc(ebx, Num(4)), esi))

  // print
  testParse(""" (eax <- (print eax))"""  -> Print(eax))
  testParse(""" (eax <- (print ebx))"""  -> Print(ebx))
  // TODO: figure out if this is even legal
  testParse(""" (eax <- (print 7))"""  -> Print(Num(7)))

  // allocate
  testParse(""" (eax <- (allocate eax 1))"""  -> Allocate(eax, Num(1)))
  testParse(""" (eax <- (allocate ebx 1))"""  -> Allocate(ebx, Num(1)))

}
