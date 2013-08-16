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
import breeze.stats.distributions.Bernoulli

trait NgramModel[T, S] {

  def n: Int

  /**
   * Calculate the probability of the given COMPLETE sequence.  This method
   * appends start and end symbols to the given sequence before calculating
   * the probability.
   *
   * So, for a trigram model, we do:
   *
   *    P(w1 | <b>,<b>) * P(w2 | <b>,w1) * P(w3 | w1,w2) * ... * P(</b> | w{n-1},wn)
   *
   * The given sequence can be any length.
   */
  def sentenceProb(sentence: Seq[T]): Double = {
    (Vector.fill(n - 1)(None) ++ sentence.map(Option(_)) :+ None)
      .sliding(n)
      .map { case context :+ word => ngramProb(context.toVector, word) }
      .product
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability.  It uses backoff models to compute the first n-1 terms
   * of the product since, for those words, there will be incomplete context.
   *
   * So, for a trigram model, we do:
   *
   *    P(w1) * P(w2 | w1) * P(w3 | w1,w2) * P(w4 | w2,w3) * P(w5 | w3,w4) * ...
   *
   * The given sequence can be any length.
   */
  def seqProb(seq: Seq[T]): Double = {
    liftedSeqProb(seq.map(Option(_)))
  }

  /**
   * Calculate the probability of the given SUB-sequence.  This method
   * DOES NOT append start or end symbols to the sequence before calculating
   * the probability, but it DOES allow start or end symbols (None) to be
   * included in the given sequence.  It uses backoff models to compute the
   * first n-1 terms of the product since, for those words, there will be
   * incomplete context.
   *
   * So, for a trigram model, we do:
   *
   *    P(w1) * P(w2 | w1) * P(w3 | w1,w2) * P(w4 | w2,w3) * P(w5 | w3,w4) * ...
   *
   * The given sequence can be any length.
   */
  def liftedSeqProb(seq: Seq[Option[T]]): Double = {
    require(seq.length >= n, "seq must have length at least N=%s".format(n))
    val backoffNgrams = seq.take(n - 1).scanLeft(Vector[Option[T]]())((z, x) => z :+ x).drop(1)
    (backoffNgrams ++ seq.sliding(n))
      .map { case context :+ word => ngramProb(context.toVector, word) }
      .product
  }

  /**
   * Get the probability of the next word given the context.
   */
  final def ngramProb(context: Vector[Option[T]], next: Option[T]): Double = {
    require(context.size <= n - 1, s"context [${context.mkString(", ")}] is longer than n-1=${n - 1}")
    _ngramProb(context, next)
  }
  protected[this] def _ngramProb(context: Vector[Option[T]], next: Option[T]): Double

  final def sample(context: Vector[Option[T]]): Option[T] = {
    require(context.size <= n - 1, s"context [${context.mkString(", ")}] is longer than n-1=${n - 1}")
    _sample(context)
  }
  protected[this] def _sample(context: Vector[Option[T]]): Option[T]

  def generate(implicit bf: CanBuildFrom[S, T, S]): S
  def generatesAs[S2]: NgramModel[T, S2]
}

final class UnigramModel[T, S](
  fd: DefaultedMultinomial[T],
  generationLength: Int = 10)
  extends NgramModel[T, S] {
  val n = 1

  final def ngramProb(next: T): Double = ngramProb(Vector(), Some(next))
  override protected[this] def _ngramProb(context: Vector[Option[T]], next: Option[T]): Double =
    next.flatMap(fd.get).getOrElse(0.0)

  final def sample: T = sample(Vector()).getOrElse(throw new AssertionError("should never happen..."))
  override protected[this] def _sample(context: Vector[Option[T]]): Option[T] =
    Some(fd.sample)

  def generate(implicit bf: CanBuildFrom[S, T, S]): S = {
    val b = bf()
    for (_ <- 1 to generationLength) b += sample
    b.result
  }
  override def generatesAs[S2]: UnigramModel[T, S2] = new UnigramModel[T, S2](fd)
}

final class OptionUnigramModel[T, S](
  fd: DefaultedMultinomial[Option[T]],
  maxGenerationLength: Int = 10)
  extends NgramModel[T, S] {
  val n = 1

  final def ngramProb(next: Option[T]): Double = ngramProb(Vector(), next)
  override protected[this] def _ngramProb(context: Vector[Option[T]], next: Option[T]): Double =
    fd.get(next).getOrElse(0.0)

  final def sample: Option[T] = sample(Vector())
  override protected[this] def _sample(context: Vector[Option[T]]): Option[T] =
    fd.sample

  def generate(implicit bf: CanBuildFrom[S, T, S]): S = {
    val b = bf()
    @tailrec def inner(cur: Vector[Option[T]], iterationsLeft: Int) {
      if (iterationsLeft > 0)
        sample(cur) match {
          case None =>
          case next =>
            b += next.get
            inner(cur.drop(1) :+ next, iterationsLeft - 1)
        }
    }
    inner(Vector.fill(n - 1)(None), maxGenerationLength)
    b.result
  }
  override def generatesAs[S2]: OptionUnigramModel[T, S2] = new OptionUnigramModel[T, S2](fd, maxGenerationLength)
}

trait HigherOrderNgramModel[T, S] extends NgramModel[T, S] {
  protected[this] def backoff: NgramModel[T, S]

  /**
   * Generate a random complete sequence based on this n-gram model.
   */
  def generate(implicit bf: CanBuildFrom[S, T, S]): S = {
    val b = bf()
    @tailrec def inner(cur: Vector[Option[T]]) {
      sample(cur) match {
        case None =>
        case next =>
          b += next.get
          inner(cur.drop(1) :+ next)
      }
    }
    inner(Vector.fill(n - 1)(None))
    b.result
  }

  override def generatesAs[S2]: HigherOrderNgramModel[T, S2]
}

final class SimpleNgramModel[T, S](
  cfd: PartialFunction[Vector[Option[T]], DefaultedMultinomial[Option[T]]],
  override protected[this] val backoff: NgramModel[T, S])
  extends HigherOrderNgramModel[T, S] {
  override val n = backoff.n + 1

  override def _ngramProb(context: Vector[Option[T]], next: Option[T]): Double =
    if (context.size == n - 1)
      cfd.lift(context.toVector).flatMap(_.get(next)).getOrElse(0.0)
    else
      backoff.ngramProb(context, next)
  override def _sample(context: Vector[Option[T]]): Option[T] =
    if (context.size == n - 1)
      cfd.lift(context).map(_.sample).getOrElse(throw new RuntimeException("couldn't sample"))
    else
      backoff.sample(context)

  override def generatesAs[S2]: SimpleNgramModel[T, S2] = new SimpleNgramModel[T, S2](cfd, backoff.generatesAs[S2])
}

final class InterpolatedBackoffNgramModel[T, S](
  cfd: PartialFunction[Vector[Option[T]], DefaultedMultinomial[Option[T]]],
  lambda: Double = 0.5,
  override protected[this] val backoff: NgramModel[T, S])
  extends HigherOrderNgramModel[T, S] {
  override val n = backoff.n + 1

  val bernoulli = Bernoulli.distribution(lambda)

  override def _ngramProb(context: Vector[Option[T]], next: Option[T]): Double =
    if (context.size == n - 1) {
      val thisProb = cfd.lift(context.toVector).flatMap(_.get(next)).getOrElse(0.0)
      val backoffProb = backoff.ngramProb(context.drop(1), next)
      (lambda * thisProb) + ((1 - lambda) * backoffProb)
    }
    else
      backoff.ngramProb(context, next)
  override def _sample(context: Vector[Option[T]]): Option[T] =
    if (context.size == n - 1)
      (if (bernoulli.draw) cfd.lift(context).map(_.sample)
      else None)
        .getOrElse(backoff.sample(context.drop(1)))
    else
      backoff.sample(context)

  override def generatesAs[S2]: InterpolatedBackoffNgramModel[T, S2] = new InterpolatedBackoffNgramModel[T, S2](cfd, lambda, backoff.generatesAs[S2])
}

final class StupidBackoffNgramModel[T, S](
  cfd: PartialFunction[Vector[Option[T]], DefaultedMultinomial[Option[T]]],
  override protected[this] val backoff: NgramModel[T, S])
  extends HigherOrderNgramModel[T, S] {
  override val n = backoff.n + 1

  override def _ngramProb(context: Vector[Option[T]], next: Option[T]): Double =
    cfd.lift(context.toVector).flatMap(_.get(next)).getOrElse(backoff.ngramProb(context.drop(1), next))
  override def _sample(context: Vector[Option[T]]): Option[T] =
    cfd.lift(context).map(_.sample).getOrElse(backoff.sample(context.drop(1)))

  override def generatesAs[S2]: StupidBackoffNgramModel[T, S2] = new StupidBackoffNgramModel[T, S2](cfd, backoff.generatesAs[S2])
}
