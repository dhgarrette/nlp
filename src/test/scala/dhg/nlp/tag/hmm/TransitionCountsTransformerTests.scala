package dhg.nlp.tag.hmm

import org.junit.Assert._
import org.junit.Test

import dhg.nlp.tag.support._
import dhg.nlp.freq._
import dhg.util.TestUtil._

class TransitionCountsTransformerTests {

  @Test
  def test_1 {
    val t = new TransitionCountsTransformer[Symbol](PassthroughCondCountsTransformer())
    val counts = DefaultedCondFreqCounts.fromMap[Option[Symbol], Option[Symbol]](
      Map[Option[Symbol], Map[Option[Symbol], Double]](
        Some('A) -> Map(Some('a) -> 1, Some('b) -> 2, None -> 3),
        Some('B) -> Map(Some('c) -> 4, Some('d) -> 5),
        None -> Map(Some('e) -> 6, Some('f) -> 7, None -> 8)))
    val result = t(counts).simpleCounts
    assertEqualsDouble(1.0, result(Some('A))(Some('a)))
    assertEqualsDouble(2.0, result(Some('A))(Some('b)))
    assertEqualsDouble(3.0, result(Some('A))(None))
    assertEqualsDouble(4.0, result(Some('B))(Some('c)))
    assertEqualsDouble(5.0, result(Some('B))(Some('d)))
    assertEquals(None, result(Some('B)).get(None))
    assertEqualsDouble(6.0, result(None)(Some('e)))
    assertEqualsDouble(7.0, result(None)(Some('f)))
    assertEqualsDouble(0.0, result(None)(None))
  }

  @Test
  def test_2 {
    val t = TransitionCountsTransformer[Symbol]()
    val counts = DefaultedCondFreqCounts.fromMap[Option[Symbol], Option[Symbol]](
      Map[Option[Symbol], Map[Option[Symbol], Double]](
        Some('A) -> Map(Some('a) -> 1, Some('b) -> 2, None -> 3),
        Some('B) -> Map(Some('c) -> 4, Some('d) -> 5),
        None -> Map(Some('e) -> 6, Some('f) -> 7)))
    val result = t(counts).simpleCounts
    assertEqualsDouble(1.0, result(Some('A))(Some('a)))
    assertEqualsDouble(2.0, result(Some('A))(Some('b)))
    assertEqualsDouble(3.0, result(Some('A))(None))
    assertEqualsDouble(4.0, result(Some('B))(Some('c)))
    assertEqualsDouble(5.0, result(Some('B))(Some('d)))
    assertEquals(None, result(Some('B)).get(None))
    assertEqualsDouble(6.0, result(None)(Some('e)))
    assertEqualsDouble(7.0, result(None)(Some('f)))
    assertEqualsDouble(0.0, result(None)(None))
  }

}
