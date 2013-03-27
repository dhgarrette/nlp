name := "nlp"

version := "1.0.0"

organization := "dhg"

scalaVersion := "2.10.1"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.4",
  "org.scalanlp" % "breeze-learn_2.10" % "0.3-SNAPSHOT" changing() excludeAll(
    ExclusionRule(organization = "com.codecommit")),
  "org.scalanlp" % "breeze-viz_2.10" % "0.3-SNAPSHOT" changing() excludeAll(
    ExclusionRule(organization = "com.codecommit")),
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default") //switch to ScalaTest at some point...

scalacOptions ++= Seq("-deprecation")
