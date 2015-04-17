package dhg.nlp.freq

import scala.collection.{ Map => CMap }
import scala.collection.breakOut

import dhg.util._
import scalaz.Scalaz._

/**
 * A builder for frequency distributions.  Stores counts (in a mutable
 * fashion) and allows counts to be added.  A distribution based on the
 * counts is generated by calling 'toFreqDist'.
 *
 * This is the top of a hierarchy designed for a modular approach to
 * frequency distribution building.  SimpleCountsTransformer serves as the basic
 * form of the counter; it stores and increments the actual counts.  Other
 * implementation of CountsTransformer will be count-transforming decorators
 * extending DelegatingCountsTransformer that wrap SimpleCountsTransformer or wrap
 * wrappers thereof.  Multiple layers of decoration allow various
 * transformations to be applied to the counts, and in varying orders.
 *
 * The operation of the system is such that counts, when added via the
 * top-most layer are passed, untouched, all the way to the base where they
 * are stored.  When toFreqDist is called, the counts are gathered via
 * recursive calls to resultCounts that travel down the layers to the bottom,
 * where the true counts are retrieved.  Each layer, starting from the bottom,
 * then applies its transformation and returns the modified counts to be
 * received by the higher layers.  Once the (modified) counts reach the top,
 * they are used to calculate the distribution.
 *
 * For example, the following code will create a CountsTransformer that, before
 * creating a distribution, will constrain its counts to those in validEntries
 * and then smooth the constrained counts:
 * {{{
 *   new SimpleSmoothingCountsTransformer(lambda,
 *     new ConstrainingCountsTransformer(validEntries, strict,
 *       new SimpleCountsTransformer()))
 * }}}
 *
 * Implementing classes should define:
 * <ul>
 *   <li> increment: Add to counts. Should simply forward to delegate.
 *   <li> resultCounts: Apply transformation to delegate's resultCounts.
 * </ul>
 *
 * @tparam B	the item being counted
 */
trait CountsTransformer[B] {
  final def apply[N](counts: CMap[B, N])(implicit num: Numeric[N]): DefaultedMultinomial[B] =
    this(DefaultedMultinomial(counts.mapVals(num.toDouble)(breakOut): Map[B, Double]))
  def apply(counts: DefaultedMultinomial[B]): DefaultedMultinomial[B]
}

//////////////////////////////////////
// Passthrough Implementation
//////////////////////////////////////

/**
 * CountsTransformer that performs no transformation
 */
case class PassthroughCountsTransformer[B]() extends CountsTransformer[B] {
  override def apply(counts: DefaultedMultinomial[B]) = counts
}

//////////////////////////////////////
// Constraining Implementation
//////////////////////////////////////

/**
 * CountsTransformer decorator that zero out counts for entries not found in
 * validEntries.
 *
 * @param validEntries	zero out entries not found in this set
 * @param delegate		the delegate counter upon which the transformation is performed
 */
case class ConstrainingCountsTransformer[B](validEntries: Set[B], delegate: CountsTransformer[B]) extends CountsTransformer[B] {
  override def apply(counts: DefaultedMultinomial[B]): DefaultedMultinomial[B] = {
    val DefaultedMultinomial(resultCounts, defaultCount, totalAddition) = delegate(counts)
    val zeroCounts = resultCounts.mapVals(_ => 0.0) // a count for every B in validEntries
    DefaultedMultinomial(validEntries.mapTo(b => resultCounts.getOrElse(b, defaultCount)).toMap |+| zeroCounts)
  }
}

object ConstrainingCountsTransformer {
  def apply[B](validEntries: Set[B]): CountsTransformer[B] =
    ConstrainingCountsTransformer(validEntries, PassthroughCountsTransformer[B]())

  /**
   * Accept validEntries as an Option.  If None: don't bother wrapping.
   */
  def apply[B](validEntriesOpt: Option[Set[B]], delegate: CountsTransformer[B]): CountsTransformer[B] =
    validEntriesOpt match {
      case Some(validEntries) => new ConstrainingCountsTransformer(validEntries, delegate)
      case None => delegate
    }

  /**
   * Accept validEntries as an Option.  If None: don't bother wrapping.
   */
  def apply[B](validEntriesOpt: Option[Set[B]]): CountsTransformer[B] =
    ConstrainingCountsTransformer(validEntriesOpt, PassthroughCountsTransformer[B]())
}

/**
 * CountsTransformer decorator that drops a specific item from the count
 *
 * @param items	the items to drop from the counts
 */
case class ItemDroppingCountsTransformer[B](items: Iterable[B], delegate: CountsTransformer[B]) extends CountsTransformer[B] {
  override def apply(counts: DefaultedMultinomial[B]) = {
    val DefaultedMultinomial(resultCounts, defaultCount, totalAddition) = delegate(counts)
    DefaultedMultinomial(resultCounts -- items, defaultCount, totalAddition)
  }
}

object ItemDroppingCountsTransformer {
  def apply[B](items: Iterable[B]): ItemDroppingCountsTransformer[B] =
    ItemDroppingCountsTransformer(items, PassthroughCountsTransformer[B]())

  /**
   * Convenience constructor just accepting one item
   *
   * @param item	the item to drop from the counts
   */
  def apply[B](item: B, delegate: CountsTransformer[B]): ItemDroppingCountsTransformer[B] =
    ItemDroppingCountsTransformer(Set(item), delegate)

  /**
   * Convenience constructor just accepting one item
   *
   * @param item	the item to drop from the counts
   */
  def apply[B](item: B): ItemDroppingCountsTransformer[B] =
    ItemDroppingCountsTransformer(Set(item), PassthroughCountsTransformer[B]())
}

//////////////////////////////////////
// Add-lambda smoothing implementation
//////////////////////////////////////

/**
 * Basic add-lambda smoothing.  A value 'lambda' is added to each count and
 * to the total count.  The 'lambda' value is also used as the default count
 * for any unseen words.
 *
 * @param lambda	smoothing parameter for add-lambda smoothing
 */
case class AddLambdaSmoothingCountsTransformer[B](lambda: Double, delegate: CountsTransformer[B]) extends CountsTransformer[B] {
  override def apply(counts: DefaultedMultinomial[B]) = {
    val DefaultedMultinomial(resultCounts, defaultCount, totalAddition) = delegate(counts)
    DefaultedMultinomial(resultCounts.mapVals(_ + lambda), defaultCount + lambda, totalAddition + lambda)
  }
}

object AddLambdaSmoothingCountsTransformer {
  def apply[B](lambda: Double): AddLambdaSmoothingCountsTransformer[B] =
    AddLambdaSmoothingCountsTransformer(lambda, PassthroughCountsTransformer[B]())
}
