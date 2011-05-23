package util

trait Timer {
  def alwaysTimed[T](message:String, f: => T): T = {
    val start = System.currentTimeMillis
    val result = f
    println("time for '"+message+"': " + (System.currentTimeMillis - start)  + " ms")
    result
  }

  def timed[T](message:String, f: => T): T =
    if(java.lang.Boolean.getBoolean("timer.display")) alwaysTimed(message, f)
    else f
}