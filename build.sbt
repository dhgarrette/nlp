name := "nlp"

version := "0.0.2-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
  "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots"
)

libraryDependencies ++= Seq(
  "dhg" %% "scala-util" % "0.0.2-SNAPSHOT",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.2.0", 
  "org.joda" % "joda-convert" % "1.2",
  "org.scalanlp" % "breeze_2.11" % "0.11.2",
  "org.scalanlp" % "breeze-natives_2.11" % "0.11.2",
  "com.github.fommil.netlib" % "core" % "1.1",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default") //switch to ScalaTest at some point...

scalacOptions ++= Seq("-deprecation")
