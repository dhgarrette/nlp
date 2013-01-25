package dhg.nlp.tag.hmm.support

import org.junit.Assert._
import org.junit.BeforeClass
import org.junit.Test

import dhg.nlp.tag._
import dhg.nlp.tag.support._
import dhg.nlp.freq._
import dhg.util.TestUtil._

class UnsupervisedEmissionDistTests {

  @Test
  def test_EstimatedRawCountUnsupervisedEmissionDistFactory() {

    val rawData = List(
      "the dog walks quickly",
      "the cat walks quietly",
      "the dog saw the cat",
      "the cat saw the dog",
      "the dog saw the saw",
      "the bird sings",
      "the mouse walks",
      "the aardvark walks",
      "the aardvark meanders").map(_.split(" ").toList)

    val tagDict = SimpleTagDict(Map(
      "bird" -> Set('N),
      "cat" -> Set('N),
      "dog" -> Set('N),
      "horse" -> Set('N),
      "mouse" -> Set('N),
      "quickly" -> Set('R),
      "quietly" -> Set('R),
      "saw" -> Set('N, 'V),
      "sings" -> Set('V),
      "the" -> Set('D),
      "walks" -> Set('V)))

    val d =
      new EstimatedRawCountUnsupervisedEmissionDistFactory[Symbol, String](
        new PassthroughCountsTransformer(),
        tagDict,
        rawData).make()

    for (w <- List("aardvark", "meanders", "horse", "unseen word", "dog", "the").map(Option(_)))
      for (t <- List('N, 'V, 'R, 'D).map(Option(_)))
        println("p(%s|%s) = %s".format(w, t, d(t)(w)))

    println
    println(d(Some('N))(Some("aardvark")) / d(Some('V))(Some("aardvark")))
    println(d(Some('N))(Some("meanders")) / d(Some('V))(Some("meanders")))

    assertEqualsDouble(1.0 / 5.0, d(Some('N))(Some("bird")))
  }

}

object UnsupervisedEmissionDistTests {

  @BeforeClass def turnOffLogging() {
    //Logger.getRootLogger.setLevel(Level.OFF)
  }

}
