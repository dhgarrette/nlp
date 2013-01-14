package dhg.hmm.tag.support

import org.junit.Assert._
import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.hmm.util.CollectionUtils._
import dhg.hmm.tag._

class ViterbiTests {

  @Test
  def test_apply() {

    type Sym = String
    type Tag = Symbol

    val fullTagset = Set('D, 'A, 'N, 'V)
    val fullTagDict = OptionalTagDict(SimpleTagDict(Map(
      "the" -> Set('D),
      "big" -> Set('A),
      "dog" -> Set('N),
      "walks" -> Set('N, 'V))))
    val incompleteTagDict = OptionalTagDict(SimpleTagDict(Map(
      "the" -> Set('D),
      "dog" -> Set('A),
      "walks" -> Set('V))))

    val largeTagTransitions = Map[Option[Tag], Set[Option[Tag]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('A), Some('N)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('N), Some('V)),
      Some('V) -> Set(None))
    val smallTagTransitions = Map[Option[Tag], Set[Option[Tag]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('A), Some('N)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('V)),
      Some('V) -> Set(None))
    val incompleteTagTransitions = Map[Option[Tag], Set[Option[Tag]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('N)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('V)),
      Some('V) -> Set(None))

    val edgeScorer = new TagEdgeScorer[Sym, Tag] {
      override def apply(prevSym: Option[Sym], prevTag: Option[Tag], currSym: Option[Sym], currTag: Option[Tag]) = {
        (prevTag, currTag) match {
          case (None, Some('D)) => 1.0
          case (Some('D), Some('A)) => .25
          case (Some('D), Some('N)) => .75
          case (Some('A), Some('N)) => 1.0
          case (Some('N), Some('N)) => .45
          case (Some('N), Some('V)) => .55
          case (Some('V), None) => 1.0
          case _ => 0.0
        }
      }
    }

    val s = "the big dog walks".split(" ").toVector

    assertEquals(Some(List('D, 'N, 'N, 'V)), new Viterbi[Sym, Tag](edgeScorer, fullTagset).tagSequence(s))
    assertEquals(Some(List('D, 'A, 'N, 'V)), new Viterbi[Sym, Tag](edgeScorer, fullTagDict).tagSequence(s))
    assertEquals(Some(List('D, 'D, 'N, 'V)), new Viterbi[Sym, Tag](edgeScorer, incompleteTagDict).tagSequence(s))
    assertEquals(None, new Viterbi[Sym, Tag](edgeScorer, NoDefaultTagDict(incompleteTagDict)).tagSequence(s))
    assertEquals(Some(List('D, 'N, 'N, 'V)), new Viterbi[Sym, Tag](edgeScorer, fullTagset, largeTagTransitions).tagSequence(s))
    assertEquals(Some(List('D, 'A, 'N, 'V)), new Viterbi[Sym, Tag](edgeScorer, fullTagDict, largeTagTransitions).tagSequence(s))
    assertEquals(Some(List('D, 'A, 'N, 'V)), new Viterbi[Sym, Tag](edgeScorer, fullTagset, smallTagTransitions).tagSequence(s))
    assertEquals(Some(List('D, 'A, 'N, 'V)), new Viterbi[Sym, Tag](edgeScorer, fullTagDict, smallTagTransitions).tagSequence(s))
    assertEquals(None, new Viterbi[Sym, Tag](edgeScorer, incompleteTagDict, largeTagTransitions).tagSequence(s))
    assertEquals(None, new Viterbi[Sym, Tag](edgeScorer, incompleteTagDict, smallTagTransitions).tagSequence(s))
    assertEquals(None, new Viterbi[Sym, Tag](edgeScorer, fullTagset, incompleteTagTransitions).tagSequence(s))
    assertEquals(None, new Viterbi[Sym, Tag](edgeScorer, fullTagDict, incompleteTagTransitions).tagSequence(s))
    assertEquals(None, new Viterbi[Sym, Tag](edgeScorer, incompleteTagDict, incompleteTagTransitions).tagSequence(s))

  }

}
