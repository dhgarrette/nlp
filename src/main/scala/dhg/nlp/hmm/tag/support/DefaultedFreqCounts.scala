package dhg.nlp.hmm.tag.support

import scalaz._
import Scalaz._

import dhg.util.CollectionUtil._

/**
 * This class stores a map of items and their counts along with a "default"
 * count (for not-yet-seen items) and a "total addition" value that is added
 * to the total count to account for the count mass of any possibly unseen
 * items.
 *
 * @tparam B	the item being counted
 */
case class DefaultedFreqCounts[B](counts: Map[B, Double], totalAddition: Double, defaultCount: Double) {
  def simpleCounts = counts
  def total = counts.values.sum + totalAddition
}

object DefaultedFreqCounts {
  def apply[B](counts: Map[B, Double]) = new DefaultedFreqCounts(counts, 0.0, 0.0)

  implicit def defaultedFreqCountsSemigroup[B]: Semigroup[DefaultedFreqCounts[B]] =
    new Semigroup[DefaultedFreqCounts[B]] {
      def append(f1: DefaultedFreqCounts[B], f2CallByName: => DefaultedFreqCounts[B]) = {
        val f2 = f2CallByName
        val countSum = f1.counts |+| f2.counts

        if (f1.totalAddition == 0.0 && f1.defaultCount == 0.0)
          new DefaultedFreqCounts(countSum, f2.totalAddition, f2.defaultCount)

        else if (f2.totalAddition == 0.0 && f2.defaultCount == 0.0)
          new DefaultedFreqCounts(countSum, f1.totalAddition, f1.defaultCount)

        else {
          assert(f1.totalAddition == f2.totalAddition)
          assert(f1.defaultCount == f2.defaultCount)
          new DefaultedFreqCounts(countSum, f1.totalAddition, f1.defaultCount)
        }
      }
    }
}

/**
 * This class stores a map of items and their counts along with a "default"
 * count (for not-yet-seen items) and a "total addition" value that is added
 * to the total count to account for the count mass of any possibly unseen
 * items.
 *
 * @tparam A	the conditioning item being counted; P(B|A).
 * @tparam B	the conditioned item being counted; P(B|A).
 */
case class DefaultedCondFreqCounts[A, B](counts: Map[A, DefaultedFreqCounts[B]]) {
  def simpleCounts = counts.mapVals(_.simpleCounts)
}

object DefaultedCondFreqCounts {
  def fromMap[A, B](counts: Map[A, Map[B, Double]]): DefaultedCondFreqCounts[A, B] =
    DefaultedCondFreqCounts(counts.mapVals(c => DefaultedFreqCounts(c)))

  implicit def defaultedCondFreqCountsSemigroup[A, B]: Semigroup[DefaultedCondFreqCounts[A, B]] =
    new Semigroup[DefaultedCondFreqCounts[A, B]] {
      def append(f1: DefaultedCondFreqCounts[A, B], f2: => DefaultedCondFreqCounts[A, B]) = {
        DefaultedCondFreqCounts(f1.counts |+| f2.counts)
      }
    }
}
