package dhg.nlp.tag.hmm

import scala.Array.canBuildFrom

import org.junit.Assert._
import org.junit.BeforeClass
import org.junit.Test

import com.typesafe.scalalogging.log4j.Logging

import dhg.nlp.tag._
import dhg.nlp.tag.support._
import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.nlp.freq._

class SemisupervisedEmHmmTaggerTrainerTests extends Logging {
  @Test
  def en_smallTagDict() {
    val (tagDictTrain, labeledTrain) = TaggedFile("data/postag/english/entrain").splitAt(3000)
    val tagDict = new SimpleTagDictFactory().make(tagDictTrain)
    val semisupervisedResults = runUnsupervisedTrainingTest(tagDict, labeledTrain)

    assertResultsEqual("""
	            Total:   91.26 (21857/23949)
				Known:   94.22 (20228/21469)
				Unknown: 65.69 (1629/2480)
				Common Mistakes:
				#Err     Gold      Model
				416      N        J       
				311      V        N       
				210      J        N       
				171      N        V       
				133      V        J       
    	""", semisupervisedResults)
  }

  @Test
  def en_smallTagDict_tdConstEmTraining() {
    // TODO: WRITE THIS TEST
  }

  @Test
  def en_smallTagDict_tdConstEmTagging() {
    // TODO: WRITE THIS TEST
  }

  @Test
  def en_smallTagDict_tdConstEmTrainingAndEmTagging() {
    // TODO: WRITE THIS TEST
  }

  private def runUnsupervisedTrainingTest(tagDict: TagDict[String, String], trainLab: Vector[Vector[(String, String)]]) = {
    val trainRaw = RawFile("data/postag/english/enraw20k")
    val gold = TaggedFile("data/postag/english/entest")

    logger.info("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    logger.info("labeledTrain.size = " + trainLab.size)
    logger.info("rawTrain.size     = " + trainRaw.size)

    val trainer: TypesupervisedTaggerTrainer[String, String] =
      new EmHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new UnconstrainedHmmTaggerFactory(tagDict.allTags),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)
    val tagger = trainer.trainWithSomeGoldLabeled(trainRaw, trainLab, tagDict)
    val output = tagger.tag(gold.map(_.map(_._1)))
    new TaggerEvaluator().evaluate(output, gold, tagDict)
  }

  object TaggedFile {
    def apply(filename: String): Vector[Vector[(String, String)]] =
      File(filename).readLines
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toTuple2).toVector)
        .toVector
  }

  object AsRawFile {
    def apply(filename: String): Vector[Vector[String]] =
      TaggedFile(filename).map(_.map(_._1))
  }

  object RawFile {
    def apply(filename: String): Vector[Vector[String]] =
      File(filename).readLines
        .map(_.trim)
        .split("###")
        .filter(_.nonEmpty)
        .map(_.toVector)
        .toVector
  }

  def assertResultsEqual(expectedString: String, results: ScoreResults[String, String]) {
    assertEquals(
      expectedString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"),
      results.toString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"))
  }
}

object SemisupervisedEmHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    //Logger.getRootLogger.setLevel(Level.OFF)
  }

}
