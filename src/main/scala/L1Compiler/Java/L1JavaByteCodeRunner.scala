package L1Compiler.Java

import L1Compiler.{FileHelper, L1Compiler, Runner}
import FileHelper._
import java.io.{OutputStream, PrintStream, File}

object L1JavaByteCodeRunner extends Runner {

  def main(args:Array[String]){ runFile(args(0)) }

  def run(code:String, originalFileName:String): String = {
    val compiler = new L1Compiler with JavaByteCodeGenerator
    val unitName = originalFileName.reverse.takeWhile(_!='/').dropWhile(_!='.').drop(1).reverse
    val jFileName = unitName + ".j"
    new File(jFileName).write(compiler.compile(code, originalFileName))
    // the compiler generates jasmin byte code, which we write to a file
    // passing that file to jasmin crealtes the class file.
    runAndCollectOutput{ jasmin.Main.main(Array(jFileName)) }
    // load the class  
    val cls = MyClassLoader(unitName, Thread.currentThread.getContextClassLoader)
    // get and run its main method
    try {
      L1JavaRuntime.newProgram()
      cls.getMethod("main", classOf[Array[String]]).invoke(cls, Array[String]())
      L1JavaRuntime.printBuffer.mkString("\n")
    }
    finally{
      "Output accumulated before error:\n" + L1JavaRuntime.printBuffer.mkString("\n")
    }
  }

  def runAndCollectOutput(f: => Unit): String = {
    // intercept System.out here for a second.
    val oldOut = System.out
    val sos = new OutputStream(){
      val buf = new StringBuilder
      def write(i: Int) = buf += i.toChar
    }
    System.setOut(new PrintStream(sos))
    // run the function
    f
    // then put the old Sytem.out back
    System.setOut(oldOut)
    sos.buf.toString
  }

  object MyClassLoader {
    def apply(name: String, parent: ClassLoader) = {
      new MyClassLoader(parent).loadClass(name)
    }

    class MyClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
      import java.io.{File, FileInputStream}
      override def findClass(name: String) = {
        val fi = new FileInputStream(new File(name + ".class"))
        val classBytes = new Array[Byte](fi.available)
        fi.read(classBytes)
        defineClass(name, classBytes, 0, classBytes.length)
      }
    }
  }
}

//import CommandRunner._
//val classpath = ".:./out/production/compilers/:./project/boot/scala-2.8.1/lib/scala-library.jar"
//runAndDieOneErrors("java -cp " + classpath + " " + unitName)
// java -cp .:./target/scala_2.8.1/classes:./project/boot/scala-2.8.1/lib/scala-library.jar Test
