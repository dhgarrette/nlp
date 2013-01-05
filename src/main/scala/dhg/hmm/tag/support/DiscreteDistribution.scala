package dhg.hmm.tag.support

import dhg.util.LogNum

trait DiscreteDistribution[T] extends (T => LogNum) {

  def sample(): T

}
