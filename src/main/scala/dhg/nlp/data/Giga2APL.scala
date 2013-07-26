package dhg.nlp.data

import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableLike
import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.Arm._
import dhg.util.CollectionUtil.KeepDelimiter._
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream

object Giga2APL {

  val DocHeadRe = """<DOC +id="(.+)" +type="(.+)" *>""".r

  def readArticles(inputFile: java.io.File) = {
    val lines =
      SelfClosingBufferedReaderIterator(GzFileBufferedReader(inputFile))
        .map(_.trim)
        .filter(_ != "(STORY CAN END HERE. OPTIONAL 2ND TAKE FOLLOWS.)")

    val articles =
      lines.splitWhere((line: String) => line.startsWith("<DOC "), KeepDelimiterAsFirst)
        .filter(_.nonEmpty)

    for (
      article <- articles;
      DocHeadRe(id, typ) = article.head;
      if Set("story").contains(typ)
    ) yield {
      val paragraphs =
        article
          .dropWhile(_ != "<TEXT>").drop(1)
          .dropRightWhile(_ != "</TEXT>").dropRight(1)
          .splitWhere(Set("<P>", "</P>"))
          .flatMap(_.split(""))
          .filter(_.nonEmpty)
          .map(_.mkString(" "))
          .toVector
      (id, typ, paragraphs)
    }
  }

  def main(args: Array[String]): Unit = {
    val (inputDir, inputFilenamePattern, outputDir) =
      args.toList match {
        case Seq(inputDir, inputFilenamePattern, outputDir) => (inputDir, inputFilenamePattern, outputDir)
        case Seq(inputDir, outputDir) => (inputDir, """.+""", outputDir)
      }
    val GzFilenameRe = s"($inputFilenamePattern)\\.gz".r

    println(s"Reading: $inputDir/$inputFilenamePattern")
    println(s"Writing: $outputDir")

    for (inputFile <- File(inputDir).ls(GzFilenameRe)) {
      val GzFilenameRe(filename) = inputFile.name
      println("Handling: " + filename)
      writeUsing(File(outputDir, s"$filename.apl")) { w =>
        for ((id, typ, paragraphs) <- readArticles(inputFile)) {
          s"${id.replaceAll("\\s+", "")}\t${typ.replaceAll("\\s+", "")}\t${paragraphs.mkString("\t")}"
        }
      }
    }
  }

}
