package dhg.hmm.tag.support

trait DiscreteDistribution[T] extends (T => Double) {

  def sample(): T

}
