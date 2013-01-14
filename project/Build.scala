import sbt._
import Keys._

object NlpBuild extends Build {

  lazy val main = Project("nlp", file(".")) dependsOn(scalautil)

  lazy val scalautil = Project("scala-util", file("scala-util"))

}
