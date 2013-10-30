object X86{
  trait Register{                                                 
   var contents = 0 
  }

  object eax extends Register                    
  object ebx extends Register                    
  object ecx extends Register
  object edx extends Register
  object edi extends Register                    
  object esi extends Register                    

  object movl{                                                    
    def apply(r1:Register, r2:Register): Unit = { r2.contents = r1.contents }
    def apply(i:Int, r:Register): Unit = { r.contents = i }
    def apply(mem:MemOffset, r:Register): Unit = { r.contents = mem.read }
    def apply(r:Register, mem:MemOffset): Unit = { mem.write(r.contents) }
  }

  object addl{                                                    
    def apply(r1:Register, r2:Register): Unit = { r2.contents += r1.contents }
    def apply(i:Int, r:Register): Unit = { r.contents += i }
  }

  object subl{                                                    
    def apply(r1:Register, r2:Register): Unit = { r2.contents -= r1.contents }
    def apply(i:Int, r:Register): Unit = { r.contents -= i }
  }

  object mull{                                                    
    def apply(r1:Register, r2:Register): Unit = { r2.contents *= r1.contents }
    def apply(i:Int, r:Register): Unit = { r.contents *= i }
  }

  object print{
    def apply(r:Register): Unit = {
      println(r.contents)
    }
  }
  
  class Label(s:String){  
    def :: = s
  }

  implicit def stringToLabel(s:String) = new Label(s)

  class MemOffset(i:Int, r:Register){
    def read = Memory.read((i/4) + r.contents)
    def write(i:Int) = Memory.write((i/4) + r.contents, i)
  }
  
  implicit def intToMemOffSet(i:Int) = new {
    def apply(r:Register) = new MemOffset(i, r)
  }

  object Memory{
    val mem = new Array[Int](100)
    def read(loc: Int) = mem(loc/4)
    def write(loc: Int, i: Int): Unit = mem(loc/4) = i
  }

  def main(args:Array[String]){
    movl (2, ebx) // move 2 into ebx
    movl (ebx, ecx) // copy ebx into ecx
    movl (5, ebx) // move 5 into ebx
    movl (ecx, 4(ebx)) // copy ecx into memory
    movl (4(ebx), eax) // copy from memory into eax
   
    addl (10, ebx)
    addl (7, edx)
  
    print (eax)
    print (ebx)
    print (ecx)
    print (edx)
    println(Memory.mem.mkString(","))
  }
}
