package dhg.nlp.tag.hmm

import org.junit.Assert._
import org.junit.BeforeClass
import org.junit.Test

import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }

import dhg.nlp.tag._
import dhg.nlp.tag.support._
import dhg.util._
import dhg.util._
import dhg.nlp.freq._

class SupervisedHmmTaggerTrainerTests extends Logging {

  @Test
  def tiny_noSmoothing() {
    val train = Vector(
      "the/D dog/N walks/V ./.",
      "the/D cat/N walks/V ./.",
      "a/D dog/N barks/V ./.")
      .map(_.split(" ").map(_.split("/").toTuple2).toVector)

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer = TransitionCountsTransformer(),
        emissionCountsTransformer = EmissionCountsTransformer(),
        hmmTaggerFactory = new UnconstrainedHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.train(train)
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("the dog walks . ".split(" ").toVector)).map(_.map(_._2)))
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("a cat walks . ".split(" ").toVector)).map(_.map(_._2)))
  }

  @Test
  def tiny_smoothed() {
    val train = Vector(
      "the/D dog/N walks/V ./.",
      "the/D cat/N walks/V ./.",
      "a/D dog/N barks/V ./.")
      .map(_.split(" ").map(_.split("/").toTuple2).toVector)

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new UnconstrainedHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.train(train)
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("the dog walks . ".split(" ").toVector)).map(_.map(_._2)))
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("a cat meows . ".split(" ").toVector)).map(_.map(_._2)))
    assertEquals(Vector(Vector("D", "N", "V", ".")), tagger.tag(Vector("this bird chirps . ".split(" ").toVector)).map(_.map(_._2)))
  }

  @Test
  def ic_noSmoothing() {
    val train = TaggedFile("data/postag/ic/ictrain.txt")

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer = TransitionCountsTransformer(),
        emissionCountsTransformer = EmissionCountsTransformer(),
        hmmTaggerFactory = new UnconstrainedHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.train(train)

    val output = tagger.tag(AsRawFile("data/postag/ic/ictest.txt"))

    val gold = TaggedFile("data/postag/ic/ictest.txt")
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		Total:   87.88 (29/33)
		Known:   87.88 (29/33)
		Unknown: NaN (0/0)
		Common Mistakes:
		#Err     Gold      Model
		3        C        H       
		1        H        C       
    	""", results)
  }

  @Test
  def en_noSmoothing() {
    val train = TaggedFile("data/postag/english/entrain")

    val tagDict = new SimpleTagDictFactory().make(train)
    val trainer: SupervisedTaggerTrainer[String, String] =
      new SupervisedHmmTaggerTrainer(
        transitionCountsTransformer = TransitionCountsTransformer(),
        emissionCountsTransformer = EmissionCountsTransformer(),
        hmmTaggerFactory = new UnconstrainedHmmTaggerFactory(tagDict.allTags))
    val tagger: Tagger[String, String] = trainer.train(train)

    val output = tagger.tag(AsRawFile("data/postag/english/entest"))

    val gold = TaggedFile("data/postag/english/entest")
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   42.18 (10101/23949)
				Known:   46.23 (10098/21841)
				Unknown: 0.14 (3/2108)
				Common Mistakes:
				#Err     Gold      Model
				4689     N        I
				1877     V        I
				1151     D        I
				1110     J        I
				1074     C        I
                """, results)
  }

  @Test
  def en_eisnerSmoothing() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = Vector(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    logger.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    logger.debug("labeledTrain.size = " + train.size)
    logger.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new UnconstrainedHmmTaggerFactory(tagDict.allTags))
    val tagger = trainer.train(train)

    //    train.flatMap(_.map(_._2).toSet).toSet
    //      .mapTo { t => tagger.emissions(Some(t))(Some("<unknown")) }
    //      .toList.sortBy(_._2).reverse
    //      .foreach { case (t, p) => println("%s \t%s".format(t, p)) }

    val output = tagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   94.02 (22517/23949)
				Known:   96.73 (21127/21841)
				Unknown: 65.94 (1390/2108)
				Common Mistakes:
				#Err     Gold      Model
				249      N        J
				228      V        N
				181      J        N
				131      N        V
				107      V        J
				""", results)
  }

  @Test
  def en_eisnerSmoothing_TagDictConstrainedTagging() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = Vector(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    logger.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    logger.debug("labeledTrain.size = " + train.size)
    logger.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new UnconstrainedHmmTaggerFactory(tagDict.allTags))
    val HmmTagger(transitions, emissions, _, _, _) = trainer.train(train)
    val constrainedTagger = HmmTagger(transitions, emissions, tagDict.opt)

    val output = constrainedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   94.15 (22548/23949)
				Known:   96.86 (21155/21841)
				Unknown: 66.08 (1393/2108)
				Common Mistakes:
				#Err     Gold      Model
				244      N        J
				232      V        N
				178      J        N
				139      N        V
				91       V        J
				""", results)
  }

  @Test
  def en_eisnerSmoothing_TagDictConstrainedTraining() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = Vector(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    logger.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    logger.debug("labeledTrain.size = " + train.size)
    logger.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          TagDictConstrainedEmissionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(tagDict.opt))
    val tagger = trainer.train(train)

    val output = tagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
				Total:   94.11 (22538/23949)
				Known:   96.83 (21149/21841)
				Unknown: 65.89 (1389/2108)
				Common Mistakes:
				#Err     Gold      Model
				249      N        J
				233      V        N
				173      J        N
				142      N        V
				96       V        J
				""", results)
  }

  @Test
  def en_eisnerSmoothing_TagDictConstrainedTrainingAndTagging() {
    val train = TaggedFile("data/postag/english/entrain")
    val tagDict = new SimpleTagDictFactory().make(train)
    val gold = TaggedFile("data/postag/english/entest")
    //val gold = Vector(Vector(("The", "D"), ("<unknown>", "N"), ("runs", "V"), (".", ".")))

    logger.debug("tagDictTrain.size = " + tagDict.setIterator.ungroup.size)
    logger.debug("labeledTrain.size = " + train.size)
    logger.debug("rawTrain.size     = " + 0)

    val trainer =
      new SupervisedHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0)),
        emissionCountsTransformer =
          TagDictConstrainedEmissionCountsTransformer(tagDict,
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(tagDict.opt))
    val HmmTagger(transitions, emissions, _, _, _) = trainer.train(train)
    val constrainedTagger = HmmTagger(transitions, emissions, tagDict.opt)

    val output = constrainedTagger.tag(gold.map(_.map(_._1)))
    val results = new TaggerEvaluator().evaluate(output, gold, tagDict)
    assertResultsEqual("""
		        Total:   94.11 (22538/23949)
				Known:   96.83 (21149/21841)
				Unknown: 65.89 (1389/2108)
				Common Mistakes:
				#Err     Gold      Model
				249      N        J
				233      V        N
				173      J        N
				142      N        V
				96       V        J
				""", results)
  }

  object TaggedFile {
    def apply(filename: String) =
      File(filename).readLines
        .map(_.trim)
        .split("###/###")
        .filter(_.nonEmpty)
        .map(_.map(_.split("/").toSeq match { case Seq(w, t) => (w, t) }).toVector)
        .toVector
  }

  object AsRawFile {
    def apply(filename: String) =
      TaggedFile(filename).map(_.map(_._1))
  }

  def assertResultsEqual(expectedString: String, results: ScoreResults[String, String]) {
    assertEquals(
      expectedString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"),
      results.toString.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n"))
  }
}

object SupervisedHmmTaggerTrainerTests {

  @BeforeClass def turnOffLogging() {
    //Logger.getRootLogger.setLevel(Level.OFF)
  }

}
