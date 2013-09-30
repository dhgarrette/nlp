name := "nlp"

version := "1.0.0-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.10.2"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
  "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots"
)

libraryDependencies ++= Seq(
  "dhg" % "scala-util_2.10" % "1.0.0-SNAPSHOT" changing(),
  "edu.stanford.nlp" % "stanford-corenlp" % "3.2.0",
  "org.scalanlp" % "breeze-math_2.10" % "0.5-SNAPSHOT",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default") //switch to ScalaTest at some point...

scalacOptions ++= Seq("-deprecation")
