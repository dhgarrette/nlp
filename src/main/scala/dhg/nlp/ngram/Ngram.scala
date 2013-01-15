package dhg.nlp.ngram

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import dhg.util.CollectionUtil._
import dhg.nlp.freq.CondCountsTransformer
import dhg.nlp.freq.PassthroughCondCountsTransformer
import dhg.nlp.freq.DefaultedMultinomial
import dhg.nlp.freq.CondFreqDist
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand

class NgramModel[T, S](
  val n: Int,
  val cfd: Seq[Option[T]] => DefaultedMultinomial[Option[T]]) {
  require(n > 0, "N must be positive")

  /**
   * Calculate the probability of the given COMPLETE sequence.  This method
   * appends start and end symbols to the given sequence before calculating
   * the probability.
   *
   * The given sequence can be any length.
   */
  def sentenceProb(sentence: Seq[T]): Double = {
    liftedSeqProb(Vector.fill(n - 1)(None) ++ sentence.map(Option(_)) :+ None)
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability.
   *
   * The given sequence must be at least N items in length.
   */
  def seqProb(seq: Seq[T]): Double = {
    liftedSeqProb(seq.map(Option(_)))
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability, but it DOES allow start or end symbols (None) to be
   * included in the given sequence.
   *
   * The given sequence must be at least N items in length.
   */
  def liftedSeqProb(seq: Seq[Option[T]]): Double = {
    require(seq.length >= n, "seq must have length at least N=%s".format(n))
    seq
      .sliding(n)
      .map { case context :+ word => cfd(context.toVector)(word) }
      .product
  }

  /**
   * Generate a random complete sequence based on this n-gram model.
   */
  def generate(implicit bf: CanBuildFrom[S, T, S]): S = {
    val b = bf()
    @tailrec def inner(cur: Seq[Option[T]]) {
      cfd(cur).sample match {
        case None =>
        case next =>
          b += next.get
          inner(cur.drop(1) :+ next)
      }
    }
    inner(Vector.fill(n - 1)(None))
    b.result
  }

  def generatesAs[S2] = new NgramModel[T, S2](n, cfd)
}

object NgramModel {
  def apply[T](n: Int, cfd: Seq[Option[T]] => DefaultedMultinomial[Option[T]]) = new NgramModel[T, Seq[T]](n, cfd)
  def unapply[T, S](ngram: NgramModel[T, S]) = Some(ngram.n, ngram.cfd)
}

case class NgramModelTrainer[T](
  n: Int,
  countsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]] = PassthroughCondCountsTransformer[Seq[Option[T]], Option[T]]())(
    implicit rand: RandBasis = Rand) {
  require(n > 0, "N must be positive")

  def apply[S <% Seq[T]](sentences: TraversableOnce[S]): NgramModel[T, S] = {
    val wordStart: Seq[Option[T]] = Vector.fill(n - 1)(None)
    val allNgrams = sentences.flatMap { sentence =>
      val ended = (wordStart ++ sentence.map(Option(_)) :+ None)
      ended.sliding(n).map { case context :+ word => context -> word }
    }
    val ngramCounts = allNgrams.toIterator.groupByKey.mapVals(_.counts)
    val transformedCounts = countsTransformer(ngramCounts)(implicitly, rand)
    new NgramModel[T, S](n, CondFreqDist(transformedCounts))
  }
}
