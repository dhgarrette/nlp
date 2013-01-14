package dhg.nlp.freq

import scala.util.Random

import dhg.util.CollectionUtil._

class MultinomialFreqDist[T](
  val distValues: Iterable[(T, Double)], default: Double = 0.0)
  extends DiscreteDistribution[T] {

  val rand = new Random

  val dist = distValues.toMap

  /*protected*/ lazy val (sampler, lastSampleKey) = {
    val vs = distValues.toIndexedSeq.filter(_._2 > 0.0).sortBy(_._2).reverse
    var running = 0.0
    val xs =
      for ((v, n) <- vs) yield {
        running += n
        v -> running
      }
    (xs, running)
  }

  override def apply(key: T) = dist.getOrElse(key, default)
  def get(key: T) = dist.get(key)
  def getNoDefault(key: T) = dist(key)
  def iterator = distValues.iterator

  /*protected*/ def findSample(key: Double) = sampler.find(_._2 >= key)

  override def sample(): T = {
    findSample(lastSampleKey * rand.nextDouble) match {
      case Some((t, _)) => t
      case None => throw new RuntimeException("Could not sample from: " + { val s = distValues.toString; if (s.length <= 50) s else s.take(47) + "..." })
    }
  }

  override def toString = "MultinomialFreqDist(%s)".format(distValues)
}

object MultinomialFreqDist {
  def apply[T](dist: Iterable[(T, Double)], default: Double = 0.0) = {
    new MultinomialFreqDist[T](dist, default)
  }

  def main(args: Array[String]) {
    {
      val values = (1 to 10).map(_ -> 1.0)
      val dist = new MultinomialFreqDist(values)
      val sampler = dist.sampler
      println(sampler)
      println(dist.lastSampleKey)
      for (x <- (0 to 10))
        println(dist.findSample(x + 0.5))
    }
    {
      val probs = Map('N -> .5, 'V -> .3, 'D -> .2)
      val dist = new MultinomialFreqDist(probs)
      println((1 to 100000).mapToVal(dist.sample).counts.normalizeValues.toVector.sortBy(-_._2))
    }
    {
      val probs = { var i = .5; (1 to 100).map(_ -> { i *= 2; 1 / i }).normalizeValues }.toIndexedSeq
      println(probs)
      val dist = new MultinomialFreqDist(probs.toMap)
      println((1 to 100000).mapToVal(dist.sample).counts.normalizeValues.toVector.sortBy(-_._2))
    }
  }
}
