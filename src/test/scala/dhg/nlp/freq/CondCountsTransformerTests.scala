package dhg.nlp.freq

import org.junit.Assert._
import org.junit._
import dhg.util.{ -> }
import dhg.nlp.util.CollectionUtils._
import dhg.util._
import dhg.util.TestUtil._

class CondCountsTransformerTests {

  @Test
  def test_PassthroughCondCountsTransformer_DefaultCounts() {
    val transformer = new PassthroughCondCountsTransformer[Char, Symbol]()

    val counts = Map(
      'A' -> Map('a -> 1.0, 'b -> 4.0),
      'B' -> Map('a -> 5.0, 'b -> 3.0),
      'C' -> Map('a -> 2.0))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsDouble(1.0 / 5.0, d('A')('a))
    assertEqualsDouble(4.0 / 5.0, d('A')('b))
    assertEqualsDouble(0.0, d('A')('def))
    assertEqualsDouble(5.0 / 8.0, d('B')('a))
    assertEqualsDouble(3.0 / 8.0, d('B')('b))
    assertEqualsDouble(0.0, d('B')('def))
    assertEqualsDouble(1.0, d('C')('a))
    assertEqualsDouble(0.0, d('C')('b))
    assertEqualsDouble(0.0, d('C')('def))
    assertEqualsDouble(8.0 / 15.0, d('Z')('a))
    assertEqualsDouble(7.0 / 15.0, d('Z')('b))
    assertEqualsDouble(0.0, d('Z')('def))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_Map() {
    val transformer =
      new ConstrainingCondCountsTransformer[Char, Symbol](validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
        zeroDefaults = true,
        delegate = MockCondCountsTransformer(
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 27.0), 0.0, 0.0),
            'C' -> DefaultedMultinomial(Map('b -> 29.0), 0.0, 0.0))),
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 4.0, 'c -> 7.0), 2.0, 3.0),
            'B' -> DefaultedMultinomial(Map('a -> 6.0, 'c -> 9.0), 3.0, 4.0),
            'C' -> DefaultedMultinomial(Map('a -> 8.0), 1.0, 5.0)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After constraining:
     *  
     *     |  A  |  B  |  C  |  D  | tot
     *   ==+=====+=====+=====+=====+=====
     *   a |  5  |  6  |  0  |  0  | 11
     *   b |  4  |  3  |  0  |  0  |  7
     *   c |  0  |  0  |  0  |  0  |  0
     *   d |  2  |  0  |  0  |  0  |  2
     * def |  0  |  0  |  0  |  0  |  0
     *   ==+=====+=====+=====+=====+=====
     *     | 11  |  9  |  0  |  0  | 20
     *     |+ 0  |  0  |  0  |  0  |  0
     * tot | 11  |  9  |  0  |  0  | 20
     * 
     */

    val counts = Map(
      'A' -> Map('a -> 27),
      'C' -> Map('b -> 29))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsDouble(5.0 / 11.0, d('A')('a))
    assertEqualsDouble(4.0 / 11.0, d('A')('b))
    assertEqualsDouble(0.0 / 11.0, d('A')('c))
    assertEqualsDouble(2.0 / 11.0, d('A')('d))
    assertEqualsDouble(0.0 / 11.0, d('A')('def))
    assertEqualsDouble(6.0 / 9.0, d('B')('a))
    assertEqualsDouble(3.0 / 9.0, d('B')('b))
    assertEqualsDouble(0.0 / 9.0, d('B')('c))
    assertEqualsDouble(0.0 / 9.0, d('B')('d))
    assertEqualsDouble(0.0 / 9.0, d('B')('def))
    assertEqualsDouble(0.0, d('C')('a))
    assertEqualsDouble(0.0, d('C')('b))
    assertEqualsDouble(0.0, d('C')('c))
    assertEqualsDouble(0.0, d('C')('d))
    assertEqualsDouble(0.0, d('C')('def))
    assertEqualsDouble(0.0, d('D')('a))
    assertEqualsDouble(0.0, d('D')('b))
    assertEqualsDouble(0.0, d('D')('c))
    assertEqualsDouble(0.0, d('D')('d))
    assertEqualsDouble(0.0, d('D')('def))
    assertEqualsDouble(11.0 / 20.0, d('Z')('a))
    assertEqualsDouble(7.0 / 20.0, d('Z')('b))
    assertEqualsDouble(0.0 / 20.0, d('Z')('c))
    assertEqualsDouble(2.0 / 20.0, d('Z')('d))
    assertEqualsDouble(0.0 / 20.0, d('Z')('def))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_DefaultCounts() {
    val transformer =
      new ConstrainingCondCountsTransformer[Char, Symbol](
        validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
        zeroDefaults = true,
        delegate = MockCondCountsTransformer(
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
            'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0))),
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 4.0, 'c -> 7.0), 2.0, 3.0),
            'B' -> DefaultedMultinomial(Map('a -> 6.0, 'c -> 9.0), 3.0, 4.0),
            'C' -> DefaultedMultinomial(Map('a -> 8.0), 1.0, 5.0)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After constraining:
     *  
     *     |  A  |  B  |  C  |  D  | tot
     *   ==+=====+=====+=====+=====+=====
     *   a |  5  |  6  |  0  |  0  | 11
     *   b |  4  |  3  |  0  |  0  |  7
     *   c |  0  |  0  |  0  |  0  |  0
     *   d |  2  |  0  |  0  |  0  |  2
     * def |  0  |  0  |  0  |  0  |  0
     *   ==+=====+=====+=====+=====+=====
     *     | 11  |  9  |  0  |  0  | 20
     *     |+ 0  |  0  |  0  |  0  |  0
     * tot | 11  |  9  |  0  |  0  | 20
     * 
     */

    val counts = DefaultedCondFreqCounts(Map(
      'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
      'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsDouble(5.0 / 11.0, d('A')('a))
    assertEqualsDouble(4.0 / 11.0, d('A')('b))
    assertEqualsDouble(0.0 / 11.0, d('A')('c))
    assertEqualsDouble(2.0 / 11.0, d('A')('d))
    assertEqualsDouble(0.0 / 11.0, d('A')('def))
    assertEqualsDouble(6.0 / 9.0, d('B')('a))
    assertEqualsDouble(3.0 / 9.0, d('B')('b))
    assertEqualsDouble(0.0 / 9.0, d('B')('c))
    assertEqualsDouble(0.0 / 9.0, d('B')('d))
    assertEqualsDouble(0.0 / 9.0, d('B')('def))
    assertEqualsDouble(0.0, d('C')('a))
    assertEqualsDouble(0.0, d('C')('b))
    assertEqualsDouble(0.0, d('C')('c))
    assertEqualsDouble(0.0, d('C')('d))
    assertEqualsDouble(0.0, d('C')('def))
    assertEqualsDouble(0.0, d('D')('a))
    assertEqualsDouble(0.0, d('D')('b))
    assertEqualsDouble(0.0, d('D')('c))
    assertEqualsDouble(0.0, d('D')('d))
    assertEqualsDouble(0.0, d('D')('def))
    assertEqualsDouble(11.0 / 20.0, d('Z')('a))
    assertEqualsDouble(7.0 / 20.0, d('Z')('b))
    assertEqualsDouble(0.0 / 20.0, d('Z')('c))
    assertEqualsDouble(2.0 / 20.0, d('Z')('d))
    assertEqualsDouble(0.0 / 20.0, d('Z')('def))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_dontZeroDefaults_DefaultCounts() {
    val transformer =
      new ConstrainingCondCountsTransformer[Char, Symbol](
        validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
        //             Map('a -> List(A, B), 'b -> List(A, B), 'd -> List(A, D))
        zeroDefaults = false,
        delegate = MockCondCountsTransformer(
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
            'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0))),
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 4.0, 'c -> 7.0), 2.0, 4.0),
            'B' -> DefaultedMultinomial(Map('a -> 6.0, 'c -> 9.0), 3.0, 3.0),
            'C' -> DefaultedMultinomial(Map('a -> 8.0), 1.0, 5.0)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 4  |  3  |  5  |  -  |
     * tot | 20  | 18  | 13  |  -  |
     * 
     * After constraining:
     *  
     *     |  A  |  B  |  C  |  D  | tot 
     *   ==+=====+=====+=====+=====+=====
     *   a |  5  |  6  |  0  |  0  | 11
     *   b |  4  |  -  |  0  |  0  |  4
     *   c |  7  |  9  |  -  |  -  | 16
     *   d |  -  |  0  |  0  |  -  |  0
     * def |  2  |  3  |  1  |  -  |  6
     *   ==+=====+=====+=====+=====+=====
     *     | 16  | 15  |  0  |  0  | 31
     *     |+ 4  |  3  |  5  |  -  | 12
     * tot | 20  | 18  |  5  |  0  | 43
     * 
     */

    val counts = DefaultedCondFreqCounts(Map(
      'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
      'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsDouble(5.0 / 20.0, d('A')('a))
    assertEqualsDouble(4.0 / 20.0, d('A')('b))
    assertEqualsDouble(7.0 / 20.0, d('A')('c))
    assertEqualsDouble(2.0 / 20.0, d('A')('d))
    assertEqualsDouble(2.0 / 20.0, d('A')('def))
    assertEqualsDouble(6.0 / 18.0, d('B')('a))
    assertEqualsDouble(3.0 / 18.0, d('B')('b))
    assertEqualsDouble(9.0 / 18.0, d('B')('c))
    assertEqualsDouble(0.0 / 18.0, d('B')('d))
    assertEqualsDouble(3.0 / 18.0, d('B')('def))
    assertEqualsDouble(0.0 / 5.0, d('C')('a))
    assertEqualsDouble(0.0 / 5.0, d('C')('b))
    assertEqualsDouble(1.0 / 5.0, d('C')('c))
    assertEqualsDouble(0.0 / 5.0, d('C')('d))
    assertEqualsDouble(1.0 / 5.0, d('C')('def))
    assertEqualsDouble(11 / 43.0, d('D')('a))
    assertEqualsDouble(4 / 43.0, d('D')('b))
    assertEqualsDouble(16 / 43.0, d('D')('c))
    assertEqualsDouble(0 / 43.0, d('D')('d))
    assertEqualsDouble(6 / 43.0, d('D')('def))
    assertEqualsDouble(11 / 43.0, d('Z')('a))
    assertEqualsDouble(4 / 43.0, d('Z')('b))
    assertEqualsDouble(16 / 43.0, d('Z')('c))
    assertEqualsDouble(0 / 43.0, d('Z')('d))
    assertEqualsDouble(6 / 43.0, d('Z')('def))
  }

  @Test
  def test_AddLambdaSmoothingCondCountsTransformer_DefaultCounts() {
    val transformer =
      AddLambdaSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.1,
        delegate = MockCondCountsTransformer(
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
            'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0))),
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 4.0, 'c -> 7.0), 2.0, 3.0),
            'B' -> DefaultedMultinomial(Map('a -> 6.0, 'c -> 9.0), 3.0, 4.0),
            'C' -> DefaultedMultinomial(Map('a -> 8.0), 1.0, 5.0)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After smoothing:
     *  
     *     |   A   |   B   |   C   |   D   |  tot
     *   ==+=======+=======+=======+=======+=======
     *   a |  5.1  |  6.1  |  8.1  |   -   | 19.3
     *   b |  4.1  |  3.1  |  1.1  |   -   |  8.3
     *   c |  7.1  |  9.1  |  1.1  |   -   | 17.3
     *   d |   -   |   -   |   -   |   -   |   - 
     * def |  2.1  |  3.1  |  1.1  |   -   |  6.3
     *   ==+=======+=======+=======+=======+=======
     *     |  16.3 |  18.3 | 10.3  |   -   | 45.9
     *     | + 3.1 |   4.1 |  5.1  |   -   | 12.3
     * tot |  19.4 |  22.4 | 15.4  |   -   | 57.2
     */

    val counts = DefaultedCondFreqCounts(Map(
      'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
      'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsDouble(5.1 / 19.4, d('A')('a))
    assertEqualsDouble(4.1 / 19.4, d('A')('b))
    assertEqualsDouble(7.1 / 19.4, d('A')('c))
    assertEqualsDouble(2.1 / 19.4, d('A')('d))
    assertEqualsDouble(2.1 / 19.4, d('A')('def))
    assertEqualsDouble(6.1 / 22.4, d('B')('a))
    assertEqualsDouble(3.1 / 22.4, d('B')('b))
    assertEqualsDouble(9.1 / 22.4, d('B')('c))
    assertEqualsDouble(3.1 / 22.4, d('B')('d))
    assertEqualsDouble(3.1 / 22.4, d('B')('def))
    assertEqualsDouble(8.1 / 15.4, d('C')('a))
    assertEqualsDouble(1.1 / 15.4, d('C')('b))
    assertEqualsDouble(1.1 / 15.4, d('C')('c))
    assertEqualsDouble(1.1 / 15.4, d('C')('d))
    assertEqualsDouble(1.1 / 15.4, d('C')('def))
    assertEqualsDouble(19.3 / 57.2, d('D')('a))
    assertEqualsDouble(8.3 / 57.2, d('D')('b))
    assertEqualsDouble(17.3 / 57.2, d('D')('c))
    assertEqualsDouble(6.3 / 57.2, d('D')('d))
    assertEqualsDouble(6.3 / 57.2, d('D')('def))
    assertEqualsDouble(19.3 / 57.2, d('Z')('a))
    assertEqualsDouble(8.3 / 57.2, d('Z')('b))
    assertEqualsDouble(17.3 / 57.2, d('Z')('c))
    assertEqualsDouble(6.3 / 57.2, d('Z')('d))
    assertEqualsDouble(6.3 / 57.2, d('Z')('def))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_before_AddLambdaSmoothingCondCountsTransformer_DefaultCounts() {
    val transformer =
      AddLambdaSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.1, delegate =
        new ConstrainingCondCountsTransformer[Char, Symbol](validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
          zeroDefaults = true,
          delegate = MockCondCountsTransformer(
            DefaultedCondFreqCounts(Map(
              'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
              'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0))),
            DefaultedCondFreqCounts(Map(
              'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 4.0, 'c -> 7.0), 2.0, 3.0),
              'B' -> DefaultedMultinomial(Map('a -> 6.0, 'c -> 9.0), 3.0, 4.0),
              'C' -> DefaultedMultinomial(Map('a -> 8.0), 1.0, 5.0))))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After constraining:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  0  |  0  |
     *   b |  4  |  3  |  0  |  0  |
     *   c |  0  |  0  |  0  |  0  |
     *   d |  2  |  0  |  0  |  0  |
     * def |  0  |  0  |  0  |  0  |
     *   ==+=====+=====+=====+=====+
     *     | 11  |  9  |  0  |  0  |
     *     |+ 0  |  0  |  0  |  0  |
     * tot | 11  |  9  |  0  |  0  |
     * 
     * After smoothing:
     *  
     *     |   A   |   B   |   C   |   D   |  tot
     *   ==+=======+=======+=======+=======+=======
     *   a |  5.1  |  6.1  |  0.1  |  0.1  | 11.4
     *   b |  4.1  |  3.1  |  0.1  |  0.1  |  7.4
     *   c |  0.1  |  0.1  |  0.1  |  0.1  |  0.4
     *   d |  2.1  |  0.1  |  0.1  |  0.1  |  2.4
     * def |  0.1  |  0.1  |  0.1  |  0.1  |  0.4
     *   ==+=======+=======+=======+=======+=======
     *     |  11.4 |  9.4  |  0.4  |  0.4  | 19.6
     *     | + 0.1 |  0.1  |  0.1  |  0.1  |  0.4
     * tot |  11.5 |  9.5  |  0.5  |  0.5  | 22.0
     */

    val counts = DefaultedCondFreqCounts(Map(
      'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
      'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsDouble(5.1 / 11.5, d('A')('a))
    assertEqualsDouble(4.1 / 11.5, d('A')('b))
    assertEqualsDouble(0.1 / 11.5, d('A')('c))
    assertEqualsDouble(2.1 / 11.5, d('A')('d))
    assertEqualsDouble(0.1 / 11.5, d('A')('def))
    assertEqualsDouble(6.1 / 9.5, d('B')('a))
    assertEqualsDouble(3.1 / 9.5, d('B')('b))
    assertEqualsDouble(0.1 / 9.5, d('B')('c))
    assertEqualsDouble(0.1 / 9.5, d('B')('d))
    assertEqualsDouble(0.1 / 9.5, d('B')('def))
    assertEqualsDouble(0.1 / 0.5, d('C')('a))
    assertEqualsDouble(0.1 / 0.5, d('C')('b))
    assertEqualsDouble(0.1 / 0.5, d('C')('c))
    assertEqualsDouble(0.1 / 0.5, d('C')('d))
    assertEqualsDouble(0.1 / 0.5, d('C')('def))
    assertEqualsDouble(0.1 / 0.5, d('D')('a))
    assertEqualsDouble(0.1 / 0.5, d('D')('b))
    assertEqualsDouble(0.1 / 0.5, d('D')('c))
    assertEqualsDouble(0.1 / 0.5, d('D')('d))
    assertEqualsDouble(0.1 / 0.5, d('D')('def))
    assertEqualsDouble(11.4 / 22.0, d('Z')('a))
    assertEqualsDouble(7.4 / 22.0, d('Z')('b))
    assertEqualsDouble(0.4 / 22.0, d('Z')('c))
    assertEqualsDouble(2.4 / 22.0, d('Z')('d))
    assertEqualsDouble(0.4 / 22.0, d('Z')('def))
  }

  @Test
  def test_AddLambdaSmoothingCondCountsTransformer_before_ConstrainingCondCountsTransformer_DefaultCounts() {
    val transformer =
      new ConstrainingCondCountsTransformer[Char, Symbol](validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
        zeroDefaults = true,
        AddLambdaSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.1,
          delegate = MockCondCountsTransformer(
            DefaultedCondFreqCounts(Map(
              'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
              'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0))),
            DefaultedCondFreqCounts(Map(
              'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 4.0, 'c -> 7.0), 2.0, 3.0),
              'B' -> DefaultedMultinomial(Map('a -> 6.0, 'c -> 9.0), 3.0, 4.0),
              'C' -> DefaultedMultinomial(Map('a -> 8.0), 1.0, 5.0))))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After smoothing:
     *  
     *     |   A   |   B   |   C   |   D   |
     *   ==+=======+=======+=======+=======+
     *   a |  5.1  |  6.1  |  8.1  |   -   |
     *   b |  4.1  |  3.1  |  1.1  |   -   |
     *   c |  7.1  |  9.1  |  1.1  |   -   |
     *   d |   -   |   -   |   -   |   -   |
     * def |  2.1  |  3.1  |  1.1  |   -   |
     *   ==+=======+=======+=======+=======+
     *     |  16.3 |  18.3 | 10.3  |   -   |
     *     | + 3.1 |   4.1 |  5.1  |   -   |
     * tot |  19.4 |  22.4 | 15.4  |   -   |
     * 
     * After constraining:
     *  
     *     |   A   |   B   |   C   |   D   |  tot
     *   ==+=======+=======+=======+=======+=======
     *   a |  5.1  |  6.1  |   0   |   0   | 11.2
     *   b |  4.1  |  3.1  |   0   |   0   |  7.2
     *   c |   0   |   0   |   0   |   0   |  0.0
     *   d |  2.1  |   0   |   0   |   0   |  2.1
     * def |   0   |   0   |   0   |   0   |  0.0
     *   ==+=======+=======+=======+=======+=======
     *     |  11.3 |  9.2  |   0   |   0   | 20.5
     *     | + 0   |   0   |   0   |   0   |  0.0
     * tot |  11.3 |  9.2  |   0   |   0   | 20.5
     * 
     */

    val counts = DefaultedCondFreqCounts(Map(
      'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
      'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsDouble(5.1 / 11.3, d('A')('a))
    assertEqualsDouble(4.1 / 11.3, d('A')('b))
    assertEqualsDouble(0.0 / 11.3, d('A')('c))
    assertEqualsDouble(2.1 / 11.3, d('A')('d))
    assertEqualsDouble(0.0 / 11.3, d('A')('def))
    assertEqualsDouble(6.1 / 9.2, d('B')('a))
    assertEqualsDouble(3.1 / 9.2, d('B')('b))
    assertEqualsDouble(0.0 / 9.2, d('B')('c))
    assertEqualsDouble(0.0 / 9.2, d('B')('d))
    assertEqualsDouble(0.0 / 9.2, d('B')('def))
    assertEqualsDouble(0.0, d('C')('a))
    assertEqualsDouble(0.0, d('C')('b))
    assertEqualsDouble(0.0, d('C')('c))
    assertEqualsDouble(0.0, d('C')('d))
    assertEqualsDouble(0.0, d('C')('def))
    assertEqualsDouble(0.0, d('D')('a))
    assertEqualsDouble(0.0, d('D')('b))
    assertEqualsDouble(0.0, d('D')('c))
    assertEqualsDouble(0.0, d('D')('d))
    assertEqualsDouble(0.0, d('D')('def))
    assertEqualsDouble(11.2 / 20.5, d('Z')('a))
    assertEqualsDouble(7.2 / 20.5, d('Z')('b))
    assertEqualsDouble(0.0 / 20.5, d('Z')('c))
    assertEqualsDouble(2.1 / 20.5, d('Z')('d))
    assertEqualsDouble(0.0 / 20.5, d('Z')('def))
  }

  @Test
  def test_EisnerSmoothingCondCountsTransformer_DefaultCounts() {
    val transformer =
      new EisnerSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.2, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 0.1),
        delegate = MockCondCountsTransformer(
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
            'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0))),
          DefaultedCondFreqCounts(Map(
            'A' -> DefaultedMultinomial(Map('a -> 5.0, 'b -> 4.0, 'c -> 7.0, 'x -> 1.0, 'y -> 1.0, 'z -> 1.0), 2.0, 3.0),
            'B' -> DefaultedMultinomial(Map('a -> 6.0, 'c -> 9.0, 'x -> 1.0, 'y -> 1.0), 3.0, 4.0),
            'C' -> DefaultedMultinomial(Map('a -> 8.0), 1.0, 5.0)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   x |  1  |  1  |  -  |  -  |
     *   y |  1  |  1  |  -  |  -  |
     *   z |  1  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     * uni |  3  |  2  |  0  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 19  | 17  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 22  | 21  | 13  |  -  |
     * 
     * Compute backoff:
     * 
     *       counts | smooth |  prob  |
     *  ===+========+========+========+
     *   a |   19   |  19.1  |        |
     *   b |    4   |   4.1  |        |
     *   c |   16   |  16.1  |        |
     *   x |    2   |   2.1  |        |
     *   y |    2   |   2.1  |        |
     *   z |    1   |   1.1  |        |
     * def |    -   |    .1  |        |
     *  ===+========+========+========+
     *     |   44   |  44.6  |        |
     *     |        | +  .1  |        |
     *     |   44   |  44.7  |        |
     * 
     * 
     * After smoothing:
     *  
     *     |             A            |            B             |             C            |            tot
     *   ==+==========================+==========================+==========================+===========================
     *   a | 5 + .2 * 3 * (19.1/44.7) | 6 + .2 * 2 * (19.1/44.7) | 8 + .2 * 0 * (19.1/44.7) | 19 + .2 * 5 * (19.1/44.7)
     *   b | 4 + .2 * 3 * ( 4.1/44.7) | 3 + .2 * 2 * ( 4.1/44.7) | 1 + .2 * 0 * ( 4.1/44.7) |  8 + .2 * 5 * ( 4.1/44.7)
     *   c | 7 + .2 * 3 * (16.1/44.7) | 9 + .2 * 2 * (16.1/44.7) | 1 + .2 * 0 * (16.1/44.7) | 17 + .2 * 5 * (16.1/44.7)
     *   x | 1 + .2 * 3 * ( 2.1/44.7) | 1 + .2 * 2 * ( 2.1/44.7) | 1 + .2 * 0 * ( 2.1/44.7) |  3 + .2 * 5 * ( 2.1/44.7)
     *   y | 1 + .2 * 3 * ( 2.1/44.7) | 1 + .2 * 2 * ( 2.1/44.7) | 1 + .2 * 0 * ( 2.1/44.7) |  3 + .2 * 5 * ( 2.1/44.7)
     *   z | 1 + .2 * 3 * ( 1.1/44.7) | 3 + .2 * 2 * ( 1.1/44.7) | 1 + .2 * 0 * ( 1.1/44.7) |  5 + .2 * 5 * ( 1.1/44.7)
     * def | 2 + .2 * 3 * (  .1/44.7) | 3 + .2 * 2 * (  .1/44.7) | 1 + .2 * 0 * (  .1/44.7) |  6 + .2 * 5 * (  .1/44.7)
     *   ==+==========================+==========================+==========================+===========================
     *     |+3 + .2 * 3 * (  .1/44.7) |+4 + .2 * 2 * (  .1/44.7) |+5 + .2 * 0 * (  .1/44.7) |+... 
     *     | 22 + .2 * 3 * ( 1 )      | 27 + .2 * 2 * ( 1 )      | 18 + .2 * 0 * ( 1 )      | 67 + .2 * 5 * ( 1 )
     *  
     */

    val counts = DefaultedCondFreqCounts(Map(
      'A' -> DefaultedMultinomial(Map('a -> 27.0), 22.0, 21.0),
      'C' -> DefaultedMultinomial(Map('b -> 29.0), 26.0, 25.0)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsDouble((5 + .2 * 3 * (19.1 / 44.7)) / (22 + .2 * 3), d('A')('a))
    assertEqualsDouble((4 + .2 * 3 * (4.1 / 44.7)) / (22 + .2 * 3), d('A')('b))
    assertEqualsDouble((7 + .2 * 3 * (16.1 / 44.7)) / (22 + .2 * 3), d('A')('c))
    assertEqualsDouble((1 + .2 * 3 * (2.1 / 44.7)) / (22 + .2 * 3), d('A')('x))
    assertEqualsDouble((1 + .2 * 3 * (2.1 / 44.7)) / (22 + .2 * 3), d('A')('y))
    assertEqualsDouble((1 + .2 * 3 * (1.1 / 44.7)) / (22 + .2 * 3), d('A')('z))
    assertEqualsDouble((2 + .2 * 3 * (.1 / 44.7)) / (22 + .2 * 3), d('A')('def))

    assertEqualsDouble((6 + .2 * 2 * (19.1 / 44.7)) / (27 + .2 * 2), d('B')('a))
    assertEqualsDouble((3 + .2 * 2 * (4.1 / 44.7)) / (27 + .2 * 2), d('B')('b))
    assertEqualsDouble((9 + .2 * 2 * (16.1 / 44.7)) / (27 + .2 * 2), d('B')('c))
    assertEqualsDouble((1 + .2 * 2 * (2.1 / 44.7)) / (27 + .2 * 2), d('B')('x))
    assertEqualsDouble((1 + .2 * 2 * (2.1 / 44.7)) / (27 + .2 * 2), d('B')('y))
    assertEqualsDouble((3 + .2 * 2 * (1.1 / 44.7)) / (27 + .2 * 2), d('B')('z))
    assertEqualsDouble((3 + .2 * 2 * (.1 / 44.7)) / (27 + .2 * 2), d('B')('def))

    assertEqualsDouble((8 + .2 * 0 * (19.1 / 44.7)) / (18 + .2 * 0), d('C')('a))
    assertEqualsDouble((1 + .2 * 0 * (4.1 / 44.7)) / (18 + .2 * 0), d('C')('b))
    assertEqualsDouble((1 + .2 * 0 * (16.1 / 44.7)) / (18 + .2 * 0), d('C')('c))
    assertEqualsDouble((1 + .2 * 0 * (2.1 / 44.7)) / (18 + .2 * 0), d('C')('x))
    assertEqualsDouble((1 + .2 * 0 * (2.1 / 44.7)) / (18 + .2 * 0), d('C')('y))
    assertEqualsDouble((1 + .2 * 0 * (1.1 / 44.7)) / (18 + .2 * 0), d('C')('z))
    assertEqualsDouble((1 + .2 * 0 * (.1 / 44.7)) / (18 + .2 * 0), d('C')('def))

    assertEqualsDouble((19 + .2 * 5 * (19.1 / 44.7)) / (67 + .2 * 5), d('Z')('a))
    assertEqualsDouble((8 + .2 * 5 * (4.1 / 44.7)) / (67 + .2 * 5), d('Z')('b))
    assertEqualsDouble((17 + .2 * 5 * (16.1 / 44.7)) / (67 + .2 * 5), d('Z')('c))
    assertEqualsDouble((3 + .2 * 5 * (2.1 / 44.7)) / (67 + .2 * 5), d('Z')('x))
    assertEqualsDouble((3 + .2 * 5 * (2.1 / 44.7)) / (67 + .2 * 5), d('Z')('y))
    assertEqualsDouble((5 + .2 * 5 * (1.1 / 44.7)) / (67 + .2 * 5), d('Z')('z))
    assertEqualsDouble((6 + .2 * 5 * (.1 / 44.7)) / (67 + .2 * 5), d('Z')('def))
  }

  case class MockCondCountsTransformer[A, B](expected: DefaultedCondFreqCounts[A, B], returned: DefaultedCondFreqCounts[A, B]) extends CondCountsTransformer[A, B] {
    override def apply(counts: DefaultedCondFreqCounts[A, B]) = {
      for ((eA -> e, cA -> c) <- (expected.counts zipSafe counts.counts)) {
        assertEquals(eA, cA)
        val DefaultedMultinomial(eC, eD, eT) = e
        val DefaultedMultinomial(cC, cD, cT) = c
        assertEquals(eC, cC)
        assertEqualsDouble(eD, cD)
        assertEqualsDouble(eT, cT)
      }
      returned
    }
  }

}

object CondCountsTransformerTests {

  @BeforeClass def turnOffLogging() {
    //Logger.getRootLogger.setLevel(Level.OFF)
  }

}
