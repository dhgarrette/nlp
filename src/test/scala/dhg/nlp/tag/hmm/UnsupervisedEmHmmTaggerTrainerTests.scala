package dhg.nlp.tag.hmm

import org.junit.Assert._
import org.junit.BeforeClass
import org.junit.Test

import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }

import dhg.nlp.tag._
import dhg.nlp.tag.hmm.support._
import dhg.nlp.tag.support._
import dhg.util._
import dhg.util._
import dhg.nlp.freq._

class UnsupervisedEmHmmTaggerTrainerTests extends Logging {

  @Test
  def tiny_beforeEM() {
    val trainRaw = Vector(
      "the dog walks quickly",
      "the cat walks quietly",
      "the dog saw the cat",
      "the cat saw the dog",
      "the dog saw the saw",
      "the bird sings",
      "the mouse walks",
      "the aardvark walks",
      "the aardvark meanders").map(_.split(" ").toVector)

    val tagDict = SimpleTagDict(Map(
      "bird" -> Set("N"),
      "cat" -> Set("N"),
      "dog" -> Set("N"),
      "horse" -> Set("N"),
      "mouse" -> Set("N"),
      "quickly" -> Set("R"),
      "quietly" -> Set("R"),
      "saw" -> Set("N", "V"),
      "sings" -> Set("V"),
      "the" -> Set("D"),
      "walks" -> Set("V")))

    val gold = Vector(
      Vector(("the", "D"), ("bird", "N"), ("walks", "V")),
      Vector(("the", "D"), ("horse", "N"), ("walks", "V")),
      Vector(("the", "D"), ("aardvark", "N"), ("walks", "V")),
      Vector(("the", "D"), ("dog", "N"), ("meanders", "V")),
      Vector(("a", "D"), ("dog", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("moose", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("runs", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("walks", "V"), ("briskly", "R")))

    // Create the initial distributions
    val initialTransitions = HmmUtils.uniformTransitionDist(tagDict.allTags)
    val initialEmissions = new EstimatedRawCountUnsupervisedEmissionDistFactory(new PassthroughCountsTransformer()).apply(trainRaw, tagDict)
    val unsupervisedTagger = HmmTagger(initialTransitions, initialEmissions, tagDict.opt)

    val output = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   85.71 (24/28)
		Known:   100.00 (22/22)
		Unknown: 33.33 (2/6)
		Common Mistakes:
		#Err     Gold      Model
		2        V        N
		1        R        N
		1        D        N
    	""", results)
  }

  @Test
  def tiny() {
    val trainRaw = Vector(
      "the aardvark walks",
      "the aardvark meanders",
      "the dog walks quickly",
      "the cat walks quietly",
      "the dog saw the cat",
      "the cat saw the dog",
      "the dog saw the saw",
      "the bird sings",
      "the mouse walks").map(_.split(" ").toVector)

    val tagDict = SimpleTagDict(Map(
      "bird" -> Set("N"),
      "cat" -> Set("N"),
      "dog" -> Set("N"),
      "horse" -> Set("N"),
      "mouse" -> Set("N"),
      "quickly" -> Set("R"),
      "quietly" -> Set("R"),
      "saw" -> Set("N", "V"),
      "sings" -> Set("V"),
      "the" -> Set("D"),
      "walks" -> Set("V")))

    val gold = Vector(
      Vector(("a", "D"), ("dog", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("bird", "N"), ("walks", "V")),
      Vector(("the", "D"), ("horse", "N"), ("walks", "V")),
      Vector(("the", "D"), ("aardvark", "N"), ("walks", "V")),
      Vector(("the", "D"), ("moose", "N"), ("walks", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("runs", "V"), ("quickly", "R")),
      Vector(("the", "D"), ("dog", "N"), ("walks", "V"), ("briskly", "R")))

    val initialHmm = HmmTagger(
      HmmUtils.uniformTransitionDist(tagDict.allTags),
      new EstimatedRawCountUnsupervisedEmissionDistFactory(new PassthroughCountsTransformer()).apply(trainRaw, tagDict),
      tagDict.opt)

    val unsupervisedTrainer: TypesupervisedTaggerTrainer[String, String] =
      new EmHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.0, AddLambdaSmoothingCountsTransformer(1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)),
        maxIterations = 1,
        minAvgLogProbChangeForEM = 0.00001)
    val unsupervisedTagger = unsupervisedTrainer.train(trainRaw, tagDict)
    val output = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   100.00 (25/25)
		Known:   100.00 (20/20)
		Unknown: 100.00 (5/5)
		Common Mistakes:
		#Err     Gold      Model
    	""", results)
  }

  @Test
  def ic_fullTagDict_noSmoothing() {
    val trainLab = TaggedFile("data/postag/ic/ictrain.txt")
    val testLab = TaggedFile("data/postag/ic/ictest.txt")

    val testRaw = AsRawFile("data/postag/ic/ictest.txt")
    val gold = TaggedFile("data/postag/ic/ictest.txt")

    val tagDict = new SimpleTagDictFactory().make(trainLab ++ testLab)

    val unsupervisedTrainer: TypesupervisedTaggerTrainer[String, String] =
      new EmHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(1.0))),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)
    val unsupervisedTagger = unsupervisedTrainer.train(trainLab.map(_.map(_._1)), tagDict)
    val output = unsupervisedTagger.tag(testRaw)
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   48.48 (16/33)
		Known:   48.48 (16/33)
		Unknown: NaN (0/0)
		Common Mistakes:
		#Err     Gold      Model
		17       H        C
    	""", results)
  }

  @Test
  def en_largeTagDict() {
    val tagDict = new SimpleTagDictFactory().make(TaggedFile("data/postag/english/entrain"))
    val unsupervisedResults = runUnsupervisedTrainingTest(tagDict)
    assertResultsEqual("""
		Total:   89.16 (21354/23949)
		Known:   92.48 (20198/21841)
		Unknown: 54.84 (1156/2108)
		Common Mistakes:
		#Err     Gold      Model
		522      D        F
		375      N        J
		223      N        V
		208      V        N
		122      J        N
    	""", unsupervisedResults)
  }

  @Test
  def en_smallTagDict() {
    val tagDict = new SimpleTagDictFactory().make(TaggedFile("data/postag/english/entrain").take(3000))
    val unsupervisedResults = runUnsupervisedTrainingTest(tagDict)
    assertResultsEqual("""
		Total:   89.98 (21549/23949)
		Known:   94.55 (20300/21469)
		Unknown: 50.36 (1249/2480)
		Common Mistakes:
		#Err     Gold      Model
		459      N        J
		230      V        N
		224      N        V
		132      N        D
		126      J        N
    	""", unsupervisedResults)
  }

  private def runUnsupervisedTrainingTest(tagDict: TagDict[String, String]) = {
    val trainRaw = RawFile("data/postag/english/enraw20k")
    val gold = TaggedFile("data/postag/english/entest")

    logger.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    logger.debug("labeledTrain.size = " + 0)
    logger.debug("rawTrain.size     = " + trainRaw.size)

    val initialHmm = HmmTagger(
      HmmUtils.uniformTransitionDist(tagDict.allTags),
      new EstimatedRawCountUnsupervisedEmissionDistFactory(new PassthroughCountsTransformer()).apply(trainRaw, tagDict),
      tagDict.opt)

    val unsupervisedTrainer: TypesupervisedHmmTaggerTrainer[String, String] =
      new EmHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(1.0))),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(OptionalTagDict(tagDict)),
        maxIterations = 20,
        minAvgLogProbChangeForEM = 0.00001)

    val unsupervisedTagger = unsupervisedTrainer.trainFromInitialHmm(trainRaw, initialHmm, tagDict)
    //val unsupervisedTagger = unsupervisedTrainer.train(trainRaw, tagDict)
    val unsupervisedOutput = unsupervisedTagger.tag(gold.map(_.map(_._1)))
    new TaggerEvaluator().evaluate(unsupervisedOutput, gold, tagDict)
  }

  object TaggedFile {
    def apply(filename: String) =
      File(filename).readLines
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toTuple2).toVector)
        .toVector
  }

  object AsRawFile {
    def apply(filename: String) =
      TaggedFile(filename).map(_.map(_._1))
  }

  object RawFile {
    def apply(filename: String) =
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

object UnsupervisedEmHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    //Logger.getRootLogger.setLevel(Level.OFF)
  }

}
