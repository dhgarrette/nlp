package dhg.hmm.tag.hmm

import scalaz._
import Scalaz._

import dhg.hmm.tag.OptionalTagDict
import dhg.hmm.tag.TagDict
import dhg.hmm.tag.support.CondCountsTransformer
import dhg.hmm.tag.support.ConstrainingCondCountsTransformer
import dhg.hmm.tag.support.DefaultedCondFreqCounts
import dhg.hmm.tag.support.DefaultedFreqCounts
import dhg.hmm.tag.support.PassthroughCondCountsTransformer
import dhg.util.CollectionUtil._

/**
 *
 */
class EmissionCountsTransformer[Tag, Sym](delegate: CondCountsTransformer[Option[Tag], Option[Sym]])
  extends CondCountsTransformer[Option[Tag], Option[Sym]] {

  override def apply(counts: DefaultedCondFreqCounts[Option[Tag], Option[Sym]]) = {
    DefaultedCondFreqCounts(
      delegate(counts).counts.map {
        case (tag, DefaultedFreqCounts(c, t, d)) =>
          tag -> (tag match {
            case None => DefaultedFreqCounts(Map((None: Option[Sym]) -> 1.0), 0.0, 0.0)
            case _ => DefaultedFreqCounts(c + (None -> 0.0), t, d)
          })
      })
  }

}

object EmissionCountsTransformer {
  def apply[Tag, Sym]() = {
    new EmissionCountsTransformer(PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }
}

/**
 * TODO: This is perhaps too constraining.  It limits the available emissions
 * to that based on the tag dictionary, but, unfortunately, that means that
 * words _not_ in the tag dictionary are impossible.  We need an option for
 * allowing the 'default' counts to not be zeroed.
 */
object TagDictConstrainedEmissionCountsTransformer {
  //  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag], allowUnseenWordTypes: Boolean): EmissionCountsTransformer[Tag, Sym] = {
  //    TagDictConstrainedEmissionCountsTransformer(tagDict, !allowUnseenWordTypes,
  //      PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  //  }

  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag], delegate: CondCountsTransformer[Option[Tag], Option[Sym]]): EmissionCountsTransformer[Tag, Sym] = {
    val c = (OptionalTagDict(tagDict).setIterator.ungroup.map(_.swap) :+ (None, None)).to[Set].groupByKey
    new EmissionCountsTransformer(
      new ConstrainingCondCountsTransformer(c, false,
        delegate))
  }
}
