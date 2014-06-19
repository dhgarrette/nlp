package dhg.nlp.ngram

import scalaz._
import Scalaz._
import org.junit.Assert._
import org.junit.Test
import breeze.stats.distributions.Multinomial
import breeze.linalg.Counter
import dhg.nlp.freq.DefaultedMultinomial
import breeze.stats.distributions.RandBasis
import dhg.nlp.freq._
import dhg.util.CollectionUtil._
import org.apache.commons.math3.random.MersenneTwister

class NgramTests {

  @Test
  def test {

    val rand = () => new RandBasis(new MersenneTwister(0))

    {
      implicit class WithPipe[T](t: T) { def ||>(block: (T => Unit)) = block(t) }

      val unitext = "to be or not to be".split("\\s+").toVector
      val optunitext = (None +: unitext.map(_.some) :+ None)
      val bitext = optunitext.sliding(2).toVector.map { case Seq(a, b) => (Vector(a) -> b) }
      val tritext = (None +: optunitext).sliding(3).toVector.map { case Seq(a, b, c) => (Vector(a, b) -> c) }

      val m1 = new UnigramModel(PassthroughCountsTransformer()(unitext.counts))
      m1 match {
        case m =>
          println(m.ngramProb("to"))
          println(m.ngramProb("be"))
          println(m.ngramProb("or"))
          println(m.ngramProb("not"))
          println(m.ngramProb("-- not a real type --"))

          println(m.seqProb("to be or not".split("\\s+").toVector))
          println(m.seqProb("to be not be".split("\\s+").toVector))

          println
      }

      val m1o = new OptionUnigramModel(PassthroughCountsTransformer().apply(optunitext.counts))
      m1o match {
        case m =>
          println(m.ngramProb(Some("to")))
          println(m.ngramProb(Some("be")))
          println(m.ngramProb(Some("or")))
          println(m.ngramProb(Some("not")))
          println(m.ngramProb(Some("-- not a real type --")))
          println(m.ngramProb(None))

          println(m.seqProb("to be or not".split("\\s+").toVector))
          println(m.seqProb("to be not be".split("\\s+").toVector))

          println
      }

      val m2 = new SimpleNgramModel[String, Nothing](CondFreqDist(PassthroughCondCountsTransformer()(bitext.groupByKey.mapVals(_.counts))), m1o)
      m2 match {
        case m =>
          println(m.ngramProb(Vector(None), Some("to")))
          println(m.ngramProb(Vector(Some("not")), Some("to")))
          println(m.ngramProb(Vector(Some("to")), Some("be")))
          println(m.ngramProb(Vector(Some("be")), Some("or")))
          println(m.ngramProb(Vector(Some("or")), Some("not")))
          println(m.ngramProb(Vector(None), Some("be")))
          println(m.ngramProb(Vector(Some("to")), Some("-- not a real type --")))
          println(m.ngramProb(Vector(Some("-- not a real type --")), Some("be")))
          println(m.ngramProb(Vector(Some("-- not a real type --")), Some("or")))

          println(m.seqProb("to be or not".split("\\s+").toVector))
          println(m.seqProb("to be not be".split("\\s+").toVector))

          println
      }

      val m2i = new InterpolatedBackoffNgramModel[String, Nothing](CondFreqDist(PassthroughCondCountsTransformer()(bitext.groupByKey.mapVals(_.counts))), 0.7, m1o)
      m2i match {
        case m =>
          println(m.ngramProb(Vector(None), Some("to")))
          println(m.ngramProb(Vector(Some("not")), Some("to")))
          println(m.ngramProb(Vector(Some("to")), Some("be")))
          println(m.ngramProb(Vector(Some("be")), Some("or")))
          println(m.ngramProb(Vector(Some("or")), Some("not")))
          println(m.ngramProb(Vector(None), Some("be")))
          println(m.ngramProb(Vector(Some("to")), Some("-- not a real type --")))
          println(m.ngramProb(Vector(Some("-- not a real type --")), Some("be")))
          println(m.ngramProb(Vector(Some("-- not a real type --")), Some("or")))

          println(m.seqProb("to be or not".split("\\s+").toVector))
          println(m.seqProb("to be not be".split("\\s+").toVector))

          println
      }

      val m2s = new StupidBackoffNgramModel[String, Nothing](CondFreqDist(PassthroughCondCountsTransformer()(bitext.groupByKey.mapVals(_.counts))), m1o)
      m2s match {
        case m =>
          println(m.ngramProb(Vector(None), Some("to")))
          println(m.ngramProb(Vector(Some("not")), Some("to")))
          println(m.ngramProb(Vector(Some("to")), Some("be")))
          println(m.ngramProb(Vector(Some("be")), Some("or")))
          println(m.ngramProb(Vector(Some("or")), Some("not")))
          println(m.ngramProb(Vector(None), Some("be")))
          println(m.ngramProb(Vector(Some("to")), Some("-- not a real type --")))
          println(m.ngramProb(Vector(Some("-- not a real type --")), Some("be")))
          println(m.ngramProb(Vector(Some("-- not a real type --")), Some("or")))

          println(m.seqProb("to be or not".split("\\s+").toVector))
          println(m.seqProb("to be not be".split("\\s+").toVector))

          println
      }

      val m3i = new InterpolatedBackoffNgramModel[String, Nothing](CondFreqDist(PassthroughCondCountsTransformer()(tritext.groupByKey.mapVals(_.counts))), 0.5, m2i)
      m3i match {
        case m =>
          println(m.ngramProb(Vector(None, None), Some("to")))
          println(m.ngramProb(Vector(None, Some("to")), Some("be")))
          println(m.ngramProb(Vector(Some("to"), Some("be")), Some("or")))
          println(m.ngramProb(Vector(Some("to"), Some("be")), None))
          println(m.ngramProb(Vector(Some("not"), Some("to")), Some("be")))
          println(m.ngramProb(Vector(Some("or"), Some("to")), Some("be")))
          println(m.ngramProb(Vector(Some("to"), Some("be")), Some("-- not a real type --")))
          println(m.ngramProb(Vector(Some("-- not a real type --"), Some("be")), Some("or")))
          println(m.ngramProb(Vector(Some("to"), Some("-- not a real type --")), Some("or")))

          println(m.seqProb("to be or not".split("\\s+").toVector))
          println(m.seqProb("to be not be".split("\\s+").toVector))

          println
      }

      //      val simpleTrainer = SimpleNgramModelTrainer[Char](3)
      //      val model = simpleTrainer(text)
      //      //model.
      //      model.generate
    }

    return

    val text = """
		Alice was beginning to get very tired of sitting by her sister on the
		bank , and of having nothing to do : once or twice she had peeped into the
		book her sister was reading , but it had no pictures or conversations in
		it , ' and what is the use of a book , ' thought Alice ' without pictures or
		conversation ?'"""

    //    //    val cfd = Map(Seq('a'.some) -> new DefaultedMultinomial(Map('a'.some -> 1.0, 'b'.some -> 2.0))(rand()))
    //    //
    //    //    val m1 = NgramModel(2, cfd) //.generatesAs[String]
    //    //    assertEquals(Seq(), m1.generate)
    //    //    assertEquals(Seq(), m1.generate)
    //    //
    //    //    val m2 = NgramModel(2, cfd).generatesAs[String]
    //    //    assertEquals("", m2.generate)
    //    //    assertEquals("", m2.generate)
    //
    //    val t3 = SimpleNgramModelTrainer[Char](3)(rand())
    //    val m3 = t3.apply(text)
    //    //println((1 to 10).map(_ => m3.generate).mkString("Vector(\"", "\", \"", "\")"))
    //    assertEquals(Vector("into", "but", "whad", ",", ":", "tires", "Alictured", "or", "to", "thersation"), (1 to 10).map(_ => m3.generate))
    //
    //    val t4 = SimpleNgramModelTrainer[Char](3, AddLambdaSmoothingCondCountsTransformer(0.01))(rand())
    //    val m4 = t4.apply(text)
    //    println((1 to 10).map(_ => m4.generate).mkString("Vector(\"", "\", \"", "\")"))
    //    //assertEquals(Vector("sher", "nosuhk", "sister", "a", "ther", "istersat", "onvui", "", "'", "was"), (1 to 10).map(_ => m4.generate))
    //
    //    val t5 = SimpleNgramModelTrainer[Char](2)(rand()) //AddLambdaSmoothingCondCountsTransformer(0.00)
    //    val m5 = t5.apply(text)
    //    //println((1 to 10).map(_ => m5.generate).mkString("Vector(\"", "\", \"", "\")"))
    //    assertEquals(Vector("ictong", "o", "wishe", "innthesad", "", "ice", "isis", "otesis", "wan", "piticotinof"), (1 to 10).map(_ => m5.generate))
    //
    //    val t6 = SimpleNgramModelTrainer[Char](4)(rand()) //AddLambdaSmoothingCondCountsTransformer(0.00)
    //    val m6 = t6.apply(text)
    //    //println((1 to 10).map(_ => m6.generate).mkString("Vector(\"", "\", \"", "\")"))
    //    assertEquals(Vector("in", "or", "book", "the", "it", "tired", "Alice", "into", "of", "twice"), (1 to 10).map(_ => m6.generate))

  }

}
