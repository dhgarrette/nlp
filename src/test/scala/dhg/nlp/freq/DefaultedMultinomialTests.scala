package dhg.nlp.freq

import org.junit.Assert._
import org.junit._
import dhg.nlp.util.CollectionUtils._
import dhg.util._
import dhg.util.TestUtil._
import breeze.stats.distributions.RandBasis
import org.apache.commons.math3.random.MersenneTwister

class DefaultedMultinomialTests {

  @Test
  def test() {

    val d = Map('a -> 2.0, 'b -> 3.0, 'c -> 5.0)
    val m1 = new DefaultedMultinomial(d)(new RandBasis(new MersenneTwister(0)))

    assertEqualsDouble(0.2, m1('a))
    assertEqualsDouble(0.2, m1.get('a).get)
    assertEqualsDouble(0.2, m1.getNoDefault('a))

    assertEqualsDouble(0.0, m1('z))
    assertEquals(None, m1.get('z))
    assertException(m1.getNoDefault('z)) { case e: RuntimeException => assertEquals("key not found: 'z", e.getMessage) }

    assertEquals(Map('a -> 188, 'b -> 304, 'c -> 508), (1 to 1000).map(_ => m1.sample).counts)

    val m2 = new DefaultedMultinomial(d, 1.0, 10.0)(new RandBasis(new MersenneTwister(0)))

    assertEqualsDouble(0.1, m2('a))
    assertEqualsDouble(0.1, m2.get('a).get)
    assertEqualsDouble(0.1, m2.getNoDefault('a))

    assertEqualsDouble(0.05, m2('z))
    assertEquals(None, m2.get('z))
    assertException(m2.getNoDefault('z)) { case e: RuntimeException => assertEquals("key not found: 'z", e.getMessage) }

    assertEquals(Map('a -> 188, 'b -> 304, 'c -> 508), (1 to 1000).map(_ => m2.sample).counts)

  }

}
