package dhg.nlp.hmm.tag.support

trait DiscreteDistribution[T] extends (T => Double) {

  def sample(): T

}
