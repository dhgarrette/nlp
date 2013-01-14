package dhg.nlp.freq

import org.junit.Assert._
import org.junit._
import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._
import dhg.nlp.test.TestUtils._
import breeze.stats.random.MersenneTwister
import breeze.stats.distributions.RandBasis

class DefaultedMultinomialTests {

  @Test
  def test() {

    val d = Map('a -> 2.0, 'b -> 3.0, 'c -> 5.0)
    val m1 = new DefaultedMultinomial(d)(new RandBasis(new MersenneTwister(0)))

    assertEqualsProb(0.2, m1('a))
    assertEqualsProb(0.2, m1.get('a).get)
    assertEqualsProb(0.2, m1.getNoDefault('a))

    assertEqualsProb(0.0, m1('z))
    assertEquals(None, m1.get('z))
    assertException(m1.getNoDefault('z)) { case e: RuntimeException => assertEquals("key not found: 'z", e.getMessage) }

    assertEquals(Map('a -> 215, 'b -> 293, 'c -> 492), (1 to 1000).map(_ => m1.sample).counts)

    val m2 = new DefaultedMultinomial(d, 1.0, 10.0)(new RandBasis(new MersenneTwister(0)))

    assertEqualsProb(0.1, m2('a))
    assertEqualsProb(0.1, m2.get('a).get)
    assertEqualsProb(0.1, m2.getNoDefault('a))

    assertEqualsProb(0.05, m2('z))
    assertEquals(None, m2.get('z))
    assertException(m2.getNoDefault('z)) { case e: RuntimeException => assertEquals("key not found: 'z", e.getMessage) }

    assertEquals(Map('a -> 215, 'b -> 293, 'c -> 492), (1 to 1000).map(_ => m2.sample).counts)

  }

}
