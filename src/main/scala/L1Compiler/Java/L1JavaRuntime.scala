package L1Compiler.Java

object L1JavaRuntime {

  // this allows us to put labels into them and other stuff...
  // maybe.
  case class Register(var value:Any = new AnyRef){
    def clear(){ value = new AnyRef }
  }
  var eax, ebx, ecx, edx, esi, ebp, esp = new Register
  val registers = List(eax, ebx, ecx, edx, esi, ebp, esp)

  val HEAP_SIZE = 1048576  // one megabyte
  val heap = new Array[Any](HEAP_SIZE)
  var allocptr = 0
  var wordsAllocated = 0
  def heapView = heap.take(wordsAllocated).toList

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

  def print(a: Any): String = a match {
    case i:Int => printContent(i) + "\n"
    case Register(v) => print(v)
    case _ => error("don't know how to print: " + a)
  }

  def printContent(in: Int, depth: Int=0): String = {
    if (depth >= 4) "..."
    else if ((in & 1) == 1) (in >> 1).toString
    else {
      val ptr = (in >> 1)
      val size = heap(ptr).asInstanceOf[Int]
      "{s:" + size + ", " +
      (for (data <- (ptr + 1) until (ptr + 1 + size)) yield heap(data) match {
        case i: Int => printContent(i, depth + 1)
        case a => error("don't know how to print: " + a)
      }).mkString(", ") + "}"
    }
  }

  // for testing
  def newProgram(){
    allocptr = 0
    wordsAllocated = 0
    for(i <- 0 until HEAP_SIZE) heap(i) = 0
    registers.foreach(_.clear())
  }

  /**
   * i ::= (x <- s)            ;; assign to a register
    | (x <- (mem x n4))   ;; read from memory @ x+n4
    | ((mem x n4) <- s)   ;; update memory @ x+n4
   */
  def mov(dest:Register, value:Any) = value match {
    case Register(v) => dest.value = v
    case _ => dest.value = value.asInstanceOf[AnyRef]
  }

  //(mem x n4))   ;; read from memory @ x+n4
  def read(x:Register, n4:Int): Any = x.value match {
    case i if i.isInstanceOf[Int] => heap(i.asInstanceOf[Int] + n4 / 4)
    case bad => error("cant read from bad memory address: " + bad)
  }

  //((mem x n4) <- s)   ;; update memory @ x+n4
  def write(x:Register, n4:Int, s:Any): Unit = x.value match {
    case i if i.isInstanceOf[Int] => heap(i.asInstanceOf[Int] + n4 / 4) = s
    case bad => error("cant write to bad memory address: " + bad)
  }

  //(x aop= s)
  // s ::= x | num | label
  //aop=  ::= += | -= | *= | &=
  def +=(x:Register, s:Any) { aop(x, s)("add", _ + _) }
  def -=(x:Register, s:Any) { aop(x, s)("minus", _ - _) }
  def *=(x:Register, s:Any) { aop(x, s)("times", _ * _) }
  def &=(x:Register, s:Any) { aop(x, s)("and", _ & _) }

  def aop(x:Register, s:Any)(name:String, f: (Int, Int) => Int) {
    s match {
      case i: Int => x.value match {
        case v if v.isInstanceOf[Int] => x.value = f(v.asInstanceOf[Int], i).asInstanceOf[AnyRef]
        case bad => error("bad contents in register for '" + name + "' operation. contents: " + bad)
      }
      case _ => error("can't add this to a register: " + s)
    }
  }
}