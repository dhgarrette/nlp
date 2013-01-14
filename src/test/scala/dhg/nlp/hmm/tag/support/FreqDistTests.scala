package dhg.nlp.hmm.tag.support

import org.junit.Assert._
import org.junit.Test

import dhg.nlp.test.TestUtils._

class FreqDistTests {

  @Test
  def test_FreqDist_empty() {
    val x = FreqDist.empty[Symbol]
    assertEqualsProb(0.0, x('z))
  }

  @Test
  def test_FreqDist_static() {
    val x = FreqDist.static[Symbol](0.5)
    assertEqualsProb(0.5, x('z))
  }

  @Test
  def test_FreqDist_apply_counts_noDefaults() {
    val x = FreqDist(DefaultedFreqCounts(Map('a -> 1.5, 'b -> 3.5), 0.0, 0.0))
    assertEqualsProb(0.3, x('a))
    assertEqualsProb(0.7, x('b))
    assertEqualsProb(0.0, x('z))
  }

  @Test
  def test_FreqDist_apply_counts_defaults() {
    val x = FreqDist(DefaultedFreqCounts(Map('a -> 3.0, 'b -> 4.0), 1.0, 2.0))
    assertEqualsProb(0.375, x('a))
    assertEqualsProb(0.500, x('b))
    assertEqualsProb(0.250, x('z))
  }

  @Test
  def test_CondFreqDist_empty() {
    val x = CondFreqDist.empty[Symbol, Symbol]
    assertEqualsProb(0.0, x('y)('z))
  }

  @Test
  def test_CondFreqDist_static() {
    val x = CondFreqDist.static[Symbol, Symbol](0.5)
    assertEqualsProb(0.5, x('y)('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_noBDefaults() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedFreqCounts(Map('a -> 5.0, 'b -> 1.0), 0.0, 0.0),
        'B' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 6.0), 0.0, 0.0))))

    assertEqualsProb(5 / 6.0, x('A')('a))
    assertEqualsProb(1 / 6.0, x('A')('b))
    assertEqualsProb(0 / 6.0, x('A')('z))
    assertEqualsProb(3 / 9.0, x('B')('a))
    assertEqualsProb(6 / 9.0, x('B')('b))
    assertEqualsProb(0 / 9.0, x('B')('z))
    assertEqualsProb(8 / 15.0, x('Z')('a))
    assertEqualsProb(7 / 15.0, x('Z')('b))
    assertEqualsProb(0 / 15.0, x('Z')('z))
  }

  @Test
  def test_CondFreqDist_apply_counts_bDefaults() {
    val x = CondFreqDist(DefaultedCondFreqCounts(
      Map(
        'A' -> DefaultedFreqCounts(Map('a -> 3.0, 'b -> 4.0), 1.0, 2.0),
        'B' -> DefaultedFreqCounts(Map('a -> 4.4, 'b -> 4.6), 1.5, 3.0))))

    assertEqualsProb(3.0 / 8.0, x('A')('a))
    assertEqualsProb(4.0 / 8.0, x('A')('b))
    assertEqualsProb(2.0 / 8.0, x('A')('z))
    assertEqualsProb(4.4 / 10.5, x('B')('a))
    assertEqualsProb(4.6 / 10.5, x('B')('b))
    assertEqualsProb(3.0 / 10.5, x('B')('z))
    assertEqualsProb(7.4 / 18.5, x('Z')('a))
    assertEqualsProb(8.6 / 18.5, x('Z')('b))
    assertEqualsProb(5.0 / 18.5, x('Z')('z))
  }

}
