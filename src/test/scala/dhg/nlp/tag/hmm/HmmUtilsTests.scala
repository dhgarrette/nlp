package dhg.nlp.tag.hmm

import org.junit.Test

import dhg.nlp.tag._
import dhg.util._
import dhg.util._

class HmmUtilsTests {

  @Test
  def test_addDistributionsToRawSequences() {
    val rawSequences = Vector(
      Vector("the", "dog", "walks"),
      Vector("the", "aardvark", "runs"))

    val tagDict = SimpleTagDict(Map(
      "the" -> Set('D),
      "dog" -> Set('N),
      "walks" -> Set('N, 'V),
      "runs" -> Set('V)), Set('D, 'A, 'N, 'V))

    val transitions = Map[Option[Symbol], Map[Option[Symbol], Double]](
      None -> Map(Some('D) -> 1.0),
      Some('D) -> Map(Some('N) -> 0.5, Some('A) -> 0.4, Some('V) -> 0.1),
      Some('A) -> Map(Some('N) -> 0.5, Some('V) -> 0.5),
      Some('N) -> Map(Some('V) -> 1.0),
      Some('V) -> Map(Some('D) -> 0.5, None -> 0.5))
      .mapVals(_.withDefaultValue(0.0))

    val emissions = Map[Option[Symbol], Map[Option[String], Double]](
      None -> Map(None -> 1.0),
      Some('D) -> Map(Some("the") -> 1.0),
      Some('A) -> Map(Some("big") -> 1.0),
      Some('N) -> Map(Some("dog") -> 0.6, Some("walks") -> 0.3, Some("aardvark") -> 0.1),
      Some('V) -> Map(Some("walks") -> 0.5, Some("runs") -> 0.5))
      .mapVals(_.withDefaultValue(0.0))

    val validTransitions = Map[Option[Symbol], Set[Option[Symbol]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('N), Some('A)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('V)),
      Some('V) -> Set(Some('D), None))

    val newSentences = HmmUtils.addDistributionsToRawSequences(rawSequences, tagDict.opt, transitions, emissions)
    for (sent <- newSentences) {
      sent foreach println
      println
    }

    val newSentencesConstrained = HmmUtils.addDistributionsToRawSequences(rawSequences, tagDict.opt, transitions, emissions, validTransitions)
    for (sent <- newSentencesConstrained) {
      sent foreach println
      println
    }

  }

  object TaggedFile {
    def apply(filename: String): Iterable[IndexedSeq[(String, String)]] = {
      val WordTagRe = """^(.+)\|([^|]+)$""".r
      new Iterable[IndexedSeq[(String, String)]] {
        override def iterator =
          File(filename).readLines
            .map(_.trim
              .split("\\s+")
              .map { case WordTagRe(word, tag) => (word, tag) }
              .toIndexedSeq)
      }
    }
  }

}
