import sbt._

class SlayonProject(info: ProjectInfo) extends DefaultProject(info) {
  // External dependencies

  val scalaToolsSnapshots = ScalaToolsSnapshots

  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"


  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}

// vim: set ts=2 sw=2 et:
