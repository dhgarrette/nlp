package dhg.hmm.tag.hmm

import org.junit.Assert._
import org.junit.Test

import dhg.hmm.tag.support._

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
    assertDouble(1.0, result(Some('A))(Some('a)))
    assertDouble(2.0, result(Some('A))(Some('b)))
    assertDouble(3.0, result(Some('A))(None))
    assertDouble(4.0, result(Some('B))(Some('c)))
    assertDouble(5.0, result(Some('B))(Some('d)))
    assertEquals(None, result(Some('B)).get(None))
    assertDouble(6.0, result(None)(Some('e)))
    assertDouble(7.0, result(None)(Some('f)))
    assertDouble(0.0, result(None)(None))
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
    assertDouble(1.0, result(Some('A))(Some('a)))
    assertDouble(2.0, result(Some('A))(Some('b)))
    assertDouble(3.0, result(Some('A))(None))
    assertDouble(4.0, result(Some('B))(Some('c)))
    assertDouble(5.0, result(Some('B))(Some('d)))
    assertEquals(None, result(Some('B)).get(None))
    assertDouble(6.0, result(None)(Some('e)))
    assertDouble(7.0, result(None)(Some('f)))
    assertDouble(0.0, result(None)(None))
  }

  private def assertDouble(expected: Double, actual: Double) {
    assertEquals(expected, actual, .000000001)
  }

}
