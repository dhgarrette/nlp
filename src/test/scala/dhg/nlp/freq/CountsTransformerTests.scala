package dhg.nlp.freq

import org.junit.Assert._
import org.junit.Test
import dhg.util.TestUtil._

class CountsTransformerTests {

  @Test
  def test_PassthroughCountsTransformer_Map_int() {
    val transformer = new PassthroughCountsTransformer[Symbol]

    val counts = Map('a -> 5, 'b -> 3)

    val d @ DefaultedMultinomial(rC, rD, rT) = transformer(counts)
    assertEqualsDouble(5.0, rC('a))
    assertEqualsDouble(3.0, rC('b))
    assertEqualsDouble(0.0, rD)
    assertEqualsDouble(0.0, rT)

    assertEqualsDouble((5.0 / 8.0), d('a))
    assertEqualsDouble((3.0 / 8.0), d('b))
    assertEqualsDouble(0.0, d('def))
  }

  @Test
  def test_PassthroughCountsTransformer_DefaultCounts_double() {
    val transformer = new PassthroughCountsTransformer[Symbol]

    val counts = DefaultedMultinomial(Map('a -> 5.0, 'b -> 3.0), 1.0, 2.0)

    val d @ DefaultedMultinomial(rC, rD, rT) = transformer(counts)
    assertEqualsDouble(5.0, rC('a))
    assertEqualsDouble(3.0, rC('b))
    assertEqualsDouble(1.0, rD)
    assertEqualsDouble(2.0, rT)

    assertEqualsDouble((5.0 / 10.0), d('a))
    assertEqualsDouble((3.0 / 10.0), d('b))
    assertEqualsDouble((1.0 / 10.0), d('def))
  }

  @Test
  def test_ConstrainingCountsTransformer_Map_int() {
    val transformer = new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
      delegate = MockCountsTransformer(
        DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 0.0, 0.0),
        DefaultedMultinomial(Map('a -> 5.0, 'b -> 3.0, 'c -> 6.0), 1.0, 2.0)))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 0.0 | 1.0 | 0.0 | 9.0 + 0 = 9.0
     * 
     */

    val counts = Map('a -> 7, 'b -> 8, 'c -> 9)

    val d @ DefaultedMultinomial(rC, rD, rT) = transformer(counts)
    assertEqualsDouble(5.0, rC('a))
    assertEqualsDouble(3.0, rC('b))
    assertEqualsDouble(0.0, rC('c))
    assertEqualsDouble(1.0, rC('d))
    assertEqualsDouble(0.0, rD)
    assertEqualsDouble(0.0, rT)

    assertEqualsDouble((5.0 / 9.0), d('a))
    assertEqualsDouble((3.0 / 9.0), d('b))
    assertEqualsDouble(0.0, d('c))
    assertEqualsDouble((1.0 / 9.0), d('d))
    assertEqualsDouble(0.0, d('def))
  }

  @Test
  def test_ConstrainingCountsTransformer_DefaultCounts_double() {
    val transformer = new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
      delegate = MockCountsTransformer(
        DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0),
        DefaultedMultinomial(Map('a -> 5.0, 'b -> 3.0, 'c -> 6.0), 1.0, 2.0)))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 0.0 | 1.0 | 0.0 | 9.0 + 0 = 9.0
     * 
     */

    val counts = DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0)

    val d @ DefaultedMultinomial(rC, rD, rT) = transformer(counts)
    assertEqualsDouble(5.0, rC('a))
    assertEqualsDouble(3.0, rC('b))
    assertEqualsDouble(0.0, rC('c))
    assertEqualsDouble(1.0, rC('d))
    assertEqualsDouble(0.0, rD)
    assertEqualsDouble(0.0, rT)

    assertEqualsDouble((5.0 / 9.0), d('a))
    assertEqualsDouble((3.0 / 9.0), d('b))
    assertEqualsDouble(0.0, d('c))
    assertEqualsDouble((1.0 / 9.0), d('d))
    assertEqualsDouble(0.0, d('def))
  }

  @Test
  def test_AddLambdaSmoothingCountsTransformer_DefaultCounts_double() {
    val transformer =
      new AddLambdaSmoothingCountsTransformer(lambda = 0.1,
        delegate = MockCountsTransformer(
          DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0),
          DefaultedMultinomial(Map('a -> 5.0, 'b -> 3.0, 'c -> 6.0), 1.0, 2.0)))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After smoothing:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 6.1 |  -  | 1.1 | 14.3 + 2.1 = 16.4
     */

    val counts = DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0)

    val d @ DefaultedMultinomial(rC, rD, rT) = transformer(counts)
    assertEqualsDouble(5.1, rC('a))
    assertEqualsDouble(3.1, rC('b))
    assertEqualsDouble(6.1, rC('c))
    assertEqualsDouble(1.1, rD)
    assertEqualsDouble(2.1, rT)

    assertEqualsDouble((5.1 / 16.4), d('a))
    assertEqualsDouble((3.1 / 16.4), d('b))
    assertEqualsDouble((6.1 / 16.4), d('c))
    assertEqualsDouble((1.1 / 16.4), d('d))
    assertEqualsDouble((1.1 / 16.4), d('def))
  }

  @Test
  def test_ConstrainingCountsTransformer_before_AddLambdaSmoothingCountsTransformer_DefaultCounts_double() {
    val transformer =
      new AddLambdaSmoothingCountsTransformer(lambda = 0.1,
        new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
          delegate = MockCountsTransformer(
            DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0),
            DefaultedMultinomial(Map('a -> 5.0, 'b -> 3.0, 'c -> 6.0), 1.0, 2.0))))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 0.0 | 1.0 | 0.0 | 9.0 + 0 = 9.0
     * 
     * After smoothing:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 0.1 | 1.1 | 0.1 | 9.4 + 0.1 = 9.5
     * 
     */

    val counts = DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0)

    val d @ DefaultedMultinomial(rC, rD, rT) = transformer(counts)
    assertEqualsDouble(5.1, rC('a))
    assertEqualsDouble(3.1, rC('b))
    assertEqualsDouble(0.1, rC('c))
    assertEqualsDouble(1.1, rC('d))
    assertEqualsDouble(0.1, rD)
    assertEqualsDouble(0.1, rT)

    assertEqualsDouble((5.1 / 9.5), d('a))
    assertEqualsDouble((3.1 / 9.5), d('b))
    assertEqualsDouble((0.1 / 9.5), d('c))
    assertEqualsDouble((1.1 / 9.5), d('d))
    assertEqualsDouble((0.1 / 9.5), d('def))
  }

  @Test
  def test_AddLambdaSmoothingCountsTransformer_before_ConstrainingCountsTransformer_DefaultCounts_double() {
    val transformer =
      new ConstrainingCountsTransformer[Symbol](validEntries = Set('a, 'b, 'd),
        new AddLambdaSmoothingCountsTransformer(lambda = 0.1,
          delegate = MockCountsTransformer(
            DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0),
            DefaultedMultinomial(Map('a -> 5.0, 'b -> 3.0, 'c -> 6.0), 1.0, 2.0))))

    /*
     * Original Counts:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.0 | 3.0 | 6.0 |  -  | 1.0 | 14.0 + 2 = 16.0
     *
     * After smoothing:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 6.1 |  -  | 1.1 | 14.3 + 2.1 = 16.4
     * 
     * After constraining:
     * 
     * |  a  |  b  |  c  |  d  | def |
     * +=====+=====+=====+=====+=====+=================
     * | 5.1 | 3.1 | 0.0 | 1.1 | 0.0 | 9.3 + 0.0 = 9.3
     * 
     */

    val counts = DefaultedMultinomial(Map('a -> 7.0, 'b -> 8.0, 'c -> 9.0), 11.0, 10.0)

    val d @ DefaultedMultinomial(rC, rD, rT) = transformer(counts)
    assertEqualsDouble(5.1, rC('a))
    assertEqualsDouble(3.1, rC('b))
    assertEqualsDouble(0.0, rC('c))
    assertEqualsDouble(1.1, rC('d))
    assertEqualsDouble(0.0, rD)
    assertEqualsDouble(0.0, rT)

    assertEqualsDouble((5.1 / 9.3), d('a))
    assertEqualsDouble((3.1 / 9.3), d('b))
    assertEqualsDouble((0.0 / 9.3), d('c))
    assertEqualsDouble((1.1 / 9.3), d('d))
    assertEqualsDouble((0.0 / 9.3), d('def))
  }

  case class MockCountsTransformer[B](expected: DefaultedMultinomial[B], returned: DefaultedMultinomial[B]) extends CountsTransformer[B] {
    override def apply(counts: DefaultedMultinomial[B]) = {
      val DefaultedMultinomial(eC, eD, eT) = expected
      val DefaultedMultinomial(cC, cD, cT) = counts
      assertEquals(eC, cC)
      assertEqualsDouble(eD, cD)
      assertEqualsDouble(eT, cT)
      returned
    }
  }

}
