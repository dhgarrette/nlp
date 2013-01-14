package dhg.hmm.tag.hmm

import scalaz._
import Scalaz._

import dhg.hmm.tag.TagDict.OptionalTagDict
import dhg.hmm.tag.TagUtils._
import dhg.hmm.tag.support.CondFreqDist
import dhg.hmm.util.CollectionUtils._
import dhg.util.CollectionUtil._

object HmmUtils {

  /**
   * Get transition and emission counts from labeled data
   *
   * @param taggedTrainSequences	labeled sequences from which to extract counts
   */
  def getCountsFromTagged[Sym, Tag](taggedSequences: Vector[Vector[(Sym, Tag)]]) = {
    // Separate symbols from tags.  Add start/final symbols and tags to each sequence
    val endedSequences = taggedSequences.map(((None -> None) +: _.map { case (s, t) => Some(s) -> Some(t) } :+ (None -> None)))
    getCountsFromEndedTagged(endedSequences)
  }

  /**
   * Get transition and emission counts from labeled data
   *
   * @param taggedTrainSequences	labeled sequences from which to extract counts
   */
  def getCountsFromEndedTagged[Sym, Tag](endedTaggedSequences: Vector[Vector[(Sym, Tag)]]) = {
    // Get the tag transitions, including start/final tags
    val tagPairs = endedTaggedSequences.map(_.map(_._2).sliding2).flatten
    val transitionCounts = tagPairs.groupByKey.mapVals(_.counts.mapVals(_.toDouble))

    // Get the word/tag pairs (emissions)
    val tagSymbolPairs = endedTaggedSequences.flatMap(_.map(_.swap))
    val emissionCounts = tagSymbolPairs.groupByKey.mapVals(_.counts.mapVals(_.toDouble))

    (transitionCounts, emissionCounts)
  }

  /**
   * Make a uniform transition distribution mapping every tag to every other
   * tag (but without mapping None -> None).
   */
  def uniformTransitionDist[Tag](tagset: Set[Tag]) = {
    val allTags: Set[Option[Tag]] = tagset.map(Some(_))
    CondFreqDist(allTags.mapToVal((allTags + None).mapToVal(1.0).toMap).toMap + (None -> allTags.mapToVal(1.0).toMap))
  }

  def addDistributionsToRawSequences[Sym, Tag](
    rawSequences: Vector[Vector[Sym]],
    tagDict: OptionalTagDict[Sym, Tag],
    transitions: Option[Tag] => Option[Tag] => Double,
    emissions: Option[Tag] => Option[Sym] => Double): Vector[Vector[(Option[Sym], Vector[(Option[Tag], (Map[Option[Tag], Double], Double))])]] = {
    val allTags = tagDict.allTags + None
    addDistributionsToRawSequences(rawSequences, tagDict, transitions, emissions, allTags.mapToVal(allTags).toMap)
  }

  def addDistributionsToRawSequences[Sym, Tag](
    rawSequences: Vector[Vector[Sym]],
    tagDict: OptionalTagDict[Sym, Tag],
    transitions: Option[Tag] => Option[Tag] => Double,
    emissions: Option[Tag] => Option[Sym] => Double,
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]): Vector[Vector[(Option[Sym], Vector[(Option[Tag], (Map[Option[Tag], Double], Double))])]] = {

    val allTags = tagDict.allTags + None
    val reverseTransitions =
      allTags
        .mapTo { prevTag =>
          val transitionsFromPrev = transitions(prevTag)
          allTags.mapTo(transitionsFromPrev)
        }
        .map { case (prevTag, currTagProbs) => (prevTag, currTagProbs.filter(tp => validTransitions(prevTag)(tp._1))) }
        .ungroup
        .map { case (prevTag, (currTag, p)) => currTag -> (prevTag -> p) }
        .groupByKey
        .mapVals(_.toMap)

    rawSequences.par.map(_.ended.map { sym =>
      val tags = tagDict.set(sym).toVector
      sym -> tags.mapTo { currTag =>
        (reverseTransitions(currTag), emissions(currTag)(sym))
      }
    }).seq

  }

}
