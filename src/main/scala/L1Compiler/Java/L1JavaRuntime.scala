package L1Compiler.Java

class L1JavaRuntime {
  /**
   *      ::= x | num | label
x     ::= cx | esi | edi | ebp | esp
cx    ::= eax | ecx | edx | ebx
sx    ::= ecs
aop=  ::= += | -= | *= | &=
sop=  ::= <<= | >>=
cmp   ::= < | <= | =
label ::= sequence of alpha-numeric characters or underscore,
          but starting with a colon, ie matching this regexp:
          #rx"^:[a-zA-Z0-9_]$"
num   ::= number between (inclusive) -2^31 and (2^31)-1
n4    ::= number between (inclusive) -2^31 and (2^31)-1
          that is divisble by 4

  ;; ebx, esi, and edi are callee / function save
  ;; eax, edx, and ecx are caller / application save / arguments (in that order)

   */

  var eax, ebx, ecx, edx, esi, ebp, esp = 0

  val HEAP_SIZE = 1048576  // one megabyte
  val heap = new Array[Int](HEAP_SIZE)

  def print(l: Int): String = printContent(l) + "\n"

  def printContent(in: Int, depth: Int=0): String = {
    if (depth >= 4) "..."
    else if ((in & 1) == 1) (in >> 1).toString
    else {
      val ptr = (in >> 1)
      val size = heap(ptr)
      "{s:" + size + ", " +
      (for (data <- (ptr + 1) until (ptr + 1 + size)) yield
        printContent(heap(data), depth + 1)).mkString(", ") +
      "}"
    }
  }

  def heapView = heap.take(wordsAllocated).toList

  var allocptr = 0
  var wordsAllocated = 0

  def allocate(fw_size: Int, fw_fill: Int): Int = {
    val size = fw_size >> 1
    val ret = allocptr
    if ((fw_size & 1) == 0)
      error("allocate called with size input that was not an encoded integer, " + fw_size)
    else if (size < 0)
      error("allocate called with size of " + size)
    else {
      allocptr += (size + 1)
      wordsAllocated += (size + 1)
      if (wordsAllocated < HEAP_SIZE) {
        heap(ret) = size
        for (i <- (ret + 1) to (ret + 1 + size)) heap(i) = fw_fill
        ret
      }
      else error("out of memory")
    }
  }

//int main() {
//  go();   // call into the generated code

}


