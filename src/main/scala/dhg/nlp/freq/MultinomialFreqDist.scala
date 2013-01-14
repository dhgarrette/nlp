package dhg.nlp.freq

import scala.util.Random
import dhg.util.CollectionUtil._
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand

class MultinomialFreqDist[T](
  val dist: Map[T, Double], default: Double = 0.0, totalAddition: Double = 0.0)(
    implicit rand: RandBasis = Rand)
  extends DiscreteDistribution[T] {
  
  private[this] val sum = dist.values.sum // sum of counts of all known events 
  private[this] val total = sum + totalAddition // sum of all counts (including unknown events)

  override def apply(key: T) = dist.getOrElse(key, default) / total
  def get(key: T) = dist.get(key).map(_ / total)
  def getNoDefault(key: T) = dist(key) / total
  def iterator = dist.iterator.mapVals(_ / total)

  override def sample(): T = {
    var key = rand.uniform.get * sum
    val itr = dist.toIterator
    while (itr.hasNext) {
      val (item, p) = itr.next()
      key -= p
      if (key <= 0) return item
    }
    throw new RuntimeException("Could not sample from: " + { val s = dist.toString; if (s.length <= 50) s else s.take(47) + "..." })
  }

  override def toString = "MultinomialFreqDist(%s)".format(dist)
}
