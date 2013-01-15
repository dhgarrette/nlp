package dhg.nlp.freq

import scala.util.Random
import dhg.util.CollectionUtil._
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand
import scalaz._
import scalaz.Scalaz._

/**
 * This class stores a map of items and their counts along with a "default"
 * count (for not-yet-seen items) and a "total addition" value that is added
 * to the total count to account for the count mass of any possibly unseen
 * items.
 *
 * Construct a frequency distribution from the counter result.  Calculates
 * the distribution by dividing each count by the total count.
 * P(B) = C(B) / Sum[C(x) for all x].
 *
 * The "totalAddition" portion is added to the total count before
 * dividing.  The "defaultCount" is used as the count for "unseen" items,
 * those items not included in the counts.
 */
case class DefaultedMultinomial[T](
  val counts: Map[T, Double], defaultCount: Double = 0.0, totalAddition: Double = 0.0)(
    implicit val rand: RandBasis = Rand)
  extends DiscreteDistribution[T] {

  private[this] val sum = counts.values.sum // sum of counts of all known events 
  private[this] val total = sum + totalAddition // sum of all counts (including unknown events)
  private[this] val isEmpty = total == 0
  private[this] val defaultProb = if (isEmpty) 0.0 else (defaultCount / total)
  private[this] val zeroProb = Some(0.0)

  /** A map of the probabilities (excluding defaults) */
  def probMap = {
    if (isEmpty)
      Map[T, Double]().withDefaultValue(0.0)
    else {
      counts.mapVals(_ / total)
    }
  }

  def simpleCounts = counts

  override def apply(key: T) = get(key).getOrElse(defaultProb)
  def get(key: T) = if (isEmpty) zeroProb else counts.get(key).map(_ / total)
  def getNoDefault(key: T) = get(key).getOrElse(sys.error(s"key not found: $key"))
  def iterator = counts.iterator.mapVals(_ / total)

  override def sample(): T = {
    var key = rand.uniform.get * sum
    val itr = counts.toIterator
    while (itr.hasNext) {
      val (item, p) = itr.next()
      if (p > 0.0) {
        key -= p
        if (key <= 0) return item
      }
    }
    throw new RuntimeException("Could not sample from: " + { val s = counts.toString; if (s.length <= 50) s else s.take(47) + "..." })
  }

  override def toString = "DefaultedMultinomial(%s, %s, %s)".format(counts, defaultCount, totalAddition)
}

object DefaultedMultinomial {
  def apply[B](counts: Map[B, Double]) = new DefaultedMultinomial(counts, 0.0, 0.0)

  implicit def DefaultedMultinomialSemigroup[B]: Semigroup[DefaultedMultinomial[B]] =
    new Semigroup[DefaultedMultinomial[B]] {
      def append(f1: DefaultedMultinomial[B], f2CallByName: => DefaultedMultinomial[B]) = {
        val f2 = f2CallByName
        val countSum = f1.counts |+| f2.counts

        if (f1.defaultCount == 0.0 && f1.totalAddition == 0.0)
          new DefaultedMultinomial(countSum, f2.defaultCount, f2.totalAddition)(f2.rand)

        else if (f2.defaultCount == 0.0 && f2.totalAddition == 0.0)
          new DefaultedMultinomial(countSum, f1.defaultCount, f1.totalAddition)(f1.rand)

        else {
          assert(f1.defaultCount == f2.defaultCount)
          assert(f1.totalAddition == f2.totalAddition)
          new DefaultedMultinomial(countSum, f1.defaultCount, f1.totalAddition)(f1.rand)
        }
      }
    }
}
