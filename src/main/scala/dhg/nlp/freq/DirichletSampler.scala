package dhg.nlp.freq

import breeze.linalg.Counter
import breeze.stats.distributions.Dirichlet
import breeze.stats.distributions.Rand
import breeze.stats.distributions.RandBasis
import dhg.util.CollectionUtil._

/**
 * This class allows a Multinomial to be sampled from a Dirichlet distribution.
 *
 * Note that even though the Dirichlet can be seeded by a Multinomial whose
 * underlying counts are arbitrarily large, the resulting sampled Multinomial
 * will have counts that sum to one.
 */
class DirichletSampler[T](dirichlet: Dirichlet[Counter[T, Double], T]) {
  def sample = DefaultedMultinomial(dirichlet.sample.toMap)
}

object DirichletSampler {

  def apply[T](labels: Iterable[T], pseudocounts: T => Double)(implicit rand: RandBasis = Rand) = {
    new DirichletSampler(new Dirichlet(Counter(labels.mapTo(pseudocounts)))(Counter.tensorspace, rand))
  }

  def apply[T](pseudocounts: Map[T, Double])(implicit rand: RandBasis = Rand) = {
    new DirichletSampler(new Dirichlet(Counter(pseudocounts))(Counter.tensorspace, rand))
  }

  def apply[T](multinomial: DefaultedMultinomial[T])(implicit rand: RandBasis = Rand) = {
    new DirichletSampler(new Dirichlet(Counter(multinomial.iterator))(Counter.tensorspace, rand))
  }

}
