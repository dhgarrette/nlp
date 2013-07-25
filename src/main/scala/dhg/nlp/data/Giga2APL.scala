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

  def main(args: Array[String]): Unit = {

    val GzFilenameRe = """(.+).gz""".r
    val DocHeadRe = """<DOC id="(.+)" type="(.+)" >""".r

    val (inputDir, outputDir) =
      args.toList match {
        case Seq(inputDir, outputDir) => (inputDir, outputDir)
      }

    for (inputFile <- File(inputDir).ls(GzFilenameRe)) {
      val GzFilenameRe(filename) = inputFile.name

      val lines =
        SelfClosingBufferedReaderIterator(GzFileBufferedReader(inputFile))
          .map(_.trim)
          .filter(_ != "(STORY CAN END HERE. OPTIONAL 2ND TAKE FOLLOWS.)")

      val articles =
        lines.splitWhere((line: String) => line.startsWith("<DOC "), KeepDelimiterAsFirst)
          .filter(_.nonEmpty)

      writeUsing(File(outputDir, s"$filename.apl")) { w =>
        articles.foreach { article =>
          val DocHeadRe(id, typ) = article.head
          val paragraphs =
            article
              .dropWhile(_ != "<P>").drop(1)
              .dropRightWhile(_ != "</P>")
              .filter(_ != "</P>")
              .split("<P>")
              .flatMap(_.split(""))
              .filter(_.nonEmpty)
              .map(_.mkString(" "))
          w.wl(s"${id.replaceAll("\\s+", "")}\t${typ.replaceAll("\\s+", "")}\t${paragraphs.mkString("\t")}")
        }
      }

    }
  }

}
