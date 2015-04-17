import sbt._
import Keys._

object CcgBuild extends Build {

  lazy val main = Project(id = "nlp", base = file(".")) dependsOn(ccg)
  
  lazy val ccg = Project("ccg", file("ccg"))

}
