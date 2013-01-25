package dhg.nlp.freq

import org.junit.Assert._
import org.junit.Test

import dhg.util.TestUtil._

class FreqDistTests {

  @Test
  def test_FreqDist_empty() {
    val x = FreqDist.empty[Symbol]
    assertEqualsDouble(0.0, x('z))
  }

  @Test
  def test_FreqDist_static() {
    val x = FreqDist.static[Symbol](0.5)
    assertEqualsDouble(0.5, x('z))
  }

  @Test
  def test_FreqDist_apply_counts_noDefaults() {
    val x = DefaultedMultinomial(Map('a -> 1.5, 'b -> 3.5), 0.0, 0.0)
    assertEqualsDouble(0.3, x('a))
    assertEqualsDouble(0.7, x('b))
    assertEqualsDouble(0.0, x('z))
  }

  @Test
  def test_FreqDist_apply_counts_defaults() {
    val x = DefaultedMultinomial(Map('a -> 3.0, 'b -> 4.0), 2.0, 1.0)
    assertEqualsDouble(0.375, x('a))
    assertEqualsDouble(0.500, x('b))
    assertEqualsDouble(0.250, x('z))
  }

  @Test
  def test_CondFreqDist_empty() {
    val x = CondFreqDist.empty[Symbol, Symbol]
    assertEqualsDouble(0.0, x('y)('z))
  }

  @Test
  def test_CondFreqDist_static() {
    val x = CondFreqDist.static[Symbol, Symbol](0.5)
    assertEqualsDouble(0.5, x('y)('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_noBDefaults() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 1.0), 0.0, 0.0),
        'B' -> DefaultedMultinomial(Map('a -> 3.0, 'b -> 6.0), 0.0, 0.0))))

    assertEqualsDouble(5 / 6.0, x('A')('a))
    assertEqualsDouble(1 / 6.0, x('A')('b))
    assertEqualsDouble(0 / 6.0, x('A')('z))
    assertEqualsDouble(3 / 9.0, x('B')('a))
    assertEqualsDouble(6 / 9.0, x('B')('b))
    assertEqualsDouble(0 / 9.0, x('B')('z))
    assertEqualsDouble(8 / 15.0, x('Z')('a))
    assertEqualsDouble(7 / 15.0, x('Z')('b))
    assertEqualsDouble(0 / 15.0, x('Z')('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_bDefaults() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedMultinomial(Map('a -> 3.0, 'b -> 4.0), 2.0, 1.0),
        'B' -> DefaultedMultinomial(Map('a -> 4.4, 'b -> 4.6), 3.0, 1.5))))

    assertEqualsDouble(3.0 / 8.0, x('A')('a))
    assertEqualsDouble(4.0 / 8.0, x('A')('b))
    assertEqualsDouble(2.0 / 8.0, x('A')('z))
    assertEqualsDouble(4.4 / 10.5, x('B')('a))
    assertEqualsDouble(4.6 / 10.5, x('B')('b))
    assertEqualsDouble(3.0 / 10.5, x('B')('z))
    assertEqualsDouble(7.4 / 18.5, x('Z')('a))
    assertEqualsDouble(8.6 / 18.5, x('Z')('b))
    assertEqualsDouble(5.0 / 18.5, x('Z')('z))
  }

}
