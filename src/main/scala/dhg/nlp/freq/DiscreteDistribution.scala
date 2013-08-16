package dhg.nlp.freq

trait DiscreteDistribution[T] extends PartialFunction[T, Double] {

  def sample(): T

}
