import sbt._

class IntepreterProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test"
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"
  val asm = "asm" % "asm-all" % "3.3.1" % "test"
  val jmock = "org.jmock" % "jmock" % "2.5.1" % "test"
  val jmockLegacy = "org.jmock" % "jmock-legacy" % "2.5.1" % "test"
}
