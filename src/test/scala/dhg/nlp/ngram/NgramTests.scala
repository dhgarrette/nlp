package dhg.nlp.ngram

import scalaz._
import Scalaz._
import org.junit.Assert._
import org.junit.Test
import breeze.stats.distributions.Multinomial
import breeze.linalg.Counter
import dhg.nlp.freq.DefaultedMultinomial
import breeze.stats.random.MersenneTwister
import breeze.stats.distributions.RandBasis
import dhg.nlp.freq.AddLambdaSmoothingCountsTransformer
import dhg.nlp.freq.AddLambdaSmoothingCondCountsTransformer

class NgramTests {

  @Test
  def test {

    val rand = () => new RandBasis(new MersenneTwister(0))

    //    val cfd = Map(Seq('a'.some) -> new DefaultedMultinomial(Map('a'.some -> 1.0, 'b'.some -> 2.0))(rand()))
    //
    //    val m1 = NgramModel(2, cfd) //.generatesAs[String]
    //    assertEquals(Seq(), m1.generate)
    //    assertEquals(Seq(), m1.generate)
    //
    //    val m2 = NgramModel(2, cfd).generatesAs[String]
    //    assertEquals("", m2.generate)
    //    assertEquals("", m2.generate)

    val text = """
		Alice was beginning to get very tired of sitting by her sister on the
		bank , and of having nothing to do : once or twice she had peeped into the
		book her sister was reading , but it had no pictures or conversations in
		it , ' and what is the use of a book , ' thought Alice ' without pictures or
		conversation ?'""".split("\\s+")

    val t3 = NgramModelTrainer[Char](3)(rand())
    val m3 = t3.apply(text)
    //println((1 to 10).map(_ => m3.generate).mkString("Vector(\"", "\", \"", "\")"))
    assertEquals(Vector("into", "but", "whad", ",", ":", "tires", "Alictured", "or", "to", "thersation"), (1 to 10).map(_ => m3.generate))

    val t4 = NgramModelTrainer[Char](3, AddLambdaSmoothingCondCountsTransformer(0.01))(rand())
    val m4 = t4.apply(text)
    println((1 to 10).map(_ => m4.generate).mkString("Vector(\"", "\", \"", "\")"))
    //assertEquals(Vector("sher", "nosuhk", "sister", "a", "ther", "istersat", "onvui", "", "'", "was"), (1 to 10).map(_ => m4.generate))

    val t5 = NgramModelTrainer[Char](2)(rand()) //AddLambdaSmoothingCondCountsTransformer(0.00)
    val m5 = t5.apply(text)
    //println((1 to 10).map(_ => m5.generate).mkString("Vector(\"", "\", \"", "\")"))
    assertEquals(Vector("ictong", "o", "wishe", "innthesad", "", "ice", "isis", "otesis", "wan", "piticotinof"), (1 to 10).map(_ => m5.generate))

    val t6 = NgramModelTrainer[Char](4)(rand()) //AddLambdaSmoothingCondCountsTransformer(0.00)
    val m6 = t6.apply(text)
    //println((1 to 10).map(_ => m6.generate).mkString("Vector(\"", "\", \"", "\")"))
    assertEquals(Vector("in", "or", "book", "the", "it", "tired", "Alice", "into", "of", "twice"), (1 to 10).map(_ => m6.generate))

  }

}
