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
import dhg.nlp.freq.CountsTransformer
import dhg.nlp.freq.PassthroughCountsTransformer

trait NgramModelTrainer[T] {
  def apply[S <% Seq[T]](sentences: Traversable[S]): NgramModel[T, S]
}

case class UnigramModelTrainer[T](
  countsTransformer: CountsTransformer[T],
  generationLength: Int = 10)(
    implicit val rand: RandBasis = Rand)
  extends NgramModelTrainer[T] {
  override def apply[S](sentences: Traversable[S])(implicit ev: S => Seq[T]): UnigramModel[T, S] =
    apply[S](sentences.toIterator)
  def apply[S](sentences: TraversableOnce[S])(implicit ev: S => Seq[T]): UnigramModel[T, S] = {
    new UnigramModel[T, S](countsTransformer(sentences.flatMap(ev).counts))
  }
}

case class OptionUnigramModelTrainer[T](
  countsTransformer: CountsTransformer[Option[T]],
  generationLength: Int = 10)(
    implicit val rand: RandBasis = Rand)
  extends NgramModelTrainer[T] {
  override def apply[S](sentences: Traversable[S])(implicit ev: S => Seq[T]): OptionUnigramModel[T, S] =
    apply[S](sentences.toIterator)
  def apply[S](sentences: TraversableOnce[S])(implicit ev: S => Seq[T]): OptionUnigramModel[T, S] = {
    val ngramCounts = sentences.flatMap(None +: _.map(Option(_)) :+ None).toIterator.counts
    new OptionUnigramModel[T, S](countsTransformer(ngramCounts), generationLength)
  }
}

trait HigherOrderNgramModelTrainer[T] extends NgramModelTrainer[T] {
  def n: Int; require(n > 0, "N must be positive")
  def condCountsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]] = PassthroughCondCountsTransformer[Seq[Option[T]], Option[T]]()

  protected def counts[S <% Seq[T]](sentences: TraversableOnce[S]) = {
    val wordStart: Seq[Option[T]] = Vector.fill(n - 1)(None)
    val allNgrams = sentences.flatMap { sentence =>
      val ended = (wordStart ++ sentence.map(Option(_)) :+ None)
      ended.sliding(n).map { case context :+ word => context -> word }
    }
    allNgrams.toIterator.groupByKey.mapVals(_.counts)
  }

  protected def condFreqDist[S <% Seq[T]](sentences: TraversableOnce[S])(implicit rand: RandBasis = Rand) =
    CondFreqDist(condCountsTransformer(counts(sentences))(implicitly, rand))
}

case class SimpleNgramModelTrainer[T](
  n: Int,
  override val condCountsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]] = PassthroughCondCountsTransformer[Seq[Option[T]], Option[T]](),
  countsTransformer: CountsTransformer[Option[T]] = PassthroughCountsTransformer[Option[T]]())(
    implicit val rand: RandBasis = Rand)
  extends HigherOrderNgramModelTrainer[T] {

  override def apply[S <% Seq[T]](sentences: Traversable[S]): NgramModel[T, S] = {
    if (n == 1)
      OptionUnigramModelTrainer[T](countsTransformer)(rand)(sentences)
    else {
      new SimpleNgramModel[T, S](condFreqDist(sentences)(implicitly, rand),
        SimpleNgramModelTrainer[T](n - 1, condCountsTransformer)(rand)(sentences))
    }
  }
}

case class InterpolatedBackoffNgramModelTrainer[T](
  n: Int,
  lambdas: Seq[Double],
  override val condCountsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]] = PassthroughCondCountsTransformer[Seq[Option[T]], Option[T]](),
  countsTransformer: CountsTransformer[Option[T]] = PassthroughCountsTransformer[Option[T]]())(
    implicit val rand: RandBasis = Rand)
  extends HigherOrderNgramModelTrainer[T] {

  override def apply[S <% Seq[T]](sentences: Traversable[S]): NgramModel[T, S] = {
    assert(lambdas.size == n, f"the given list of lambdas [${lambdas.map(l => f"$l%.2f").mkString(", ")}] must be length n=$n")
    if (n == 1)
      OptionUnigramModelTrainer[T](countsTransformer)(rand)(sentences)
    else {
      val lambda +: remainingLambdas = lambdas.normalize
      new InterpolatedBackoffNgramModel[T, S](condFreqDist(sentences)(implicitly, rand), lambda,
        InterpolatedBackoffNgramModelTrainer[T](n - 1, remainingLambdas, condCountsTransformer, countsTransformer)(rand)(sentences))
    }
  }
}

object InterpolatedBackoffNgramModelTrainer {
  /**
   * Use lambdas that get cut in half each backoff: [n^2, (n-1)^1, ..., 2, 1]
   */
  def makeWithImplicitLambdas[T](
    n: Int,
    condCountsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]] = PassthroughCondCountsTransformer[Seq[Option[T]], Option[T]](),
    countsTransformer: CountsTransformer[Option[T]] = PassthroughCountsTransformer[Option[T]]())(
      implicit rand: RandBasis = Rand) =
    InterpolatedBackoffNgramModelTrainer(n, List.tabulate(n)(i => math.pow(2, n - i - 1)), condCountsTransformer, countsTransformer)(rand)
}

case class StupidBackoffNgramModelTrainer[T](
  n: Int,
  override val condCountsTransformer: CondCountsTransformer[Seq[Option[T]], Option[T]] = PassthroughCondCountsTransformer[Seq[Option[T]], Option[T]](),
  countsTransformer: CountsTransformer[Option[T]] = PassthroughCountsTransformer[Option[T]]())(
    implicit val rand: RandBasis = Rand)
  extends HigherOrderNgramModelTrainer[T] {

  override def apply[S <% Seq[T]](sentences: Traversable[S]): NgramModel[T, S] = {
    if (n == 1)
      OptionUnigramModelTrainer[T](countsTransformer)(rand)(sentences)
    else {
      new StupidBackoffNgramModel[T, S](condFreqDist(sentences)(implicitly, rand),
        StupidBackoffNgramModelTrainer[T](n - 1, condCountsTransformer, countsTransformer)(rand)(sentences))
    }
  }
}
