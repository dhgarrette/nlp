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
import dhg.nlp.data.AnnotatedData.AnnotatedToken

object Giga2TokLemPos {

  def main(args: Array[String]): Unit = {
    val (inputDir, inputFilenamePattern, outputDir) =
      args.toList match {
        case Seq(inputDir, inputFilenamePattern, outputDir) => (inputDir, inputFilenamePattern, outputDir)
        case Seq(inputDir, outputDir) => (inputDir, """.+""", outputDir)
      }
    val InputFilenameRe = s"($inputFilenamePattern)\\.gz".r

    println(s"Reading: $inputDir/$inputFilenamePattern")
    println(s"Writing: $outputDir")

    val annotator = new StanfordAnnotator()

    for (inputFile <- File(inputDir).ls(InputFilenameRe)) {
      val InputFilenameRe(filename) = inputFile.name
      println("Handling: " + filename)
      writeUsing(File(outputDir, s"$filename.tlp")) { w =>
        for ((id, typ, paragraphs) <- Giga2APL.readArticles(inputFile)) {
          val annotatedDoc = annotator(paragraphs)
          val sentenceStrings = annotatedDoc.map(s => s.map { case AnnotatedToken(CleanTok(w), CleanTok(l), p, _) => s"$w|$l|$p" }.mkString(" "))
          w.wl(s"${id.replaceAll("\\s+", "")}\t${typ.replaceAll("\\s+", "")}\t${sentenceStrings.mkString("\t")}")
        }
      }
    }
  }

  object CleanTok {
    def unapply(t: String): Option[String] = Some(t match {
      case "|" => "-VERTBAR-"
      case _ => t
    })
  }

}
