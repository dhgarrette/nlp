package dhg.nlp.classify

import scala.collection.mutable.{ Map => MMap }
import dhg.util._
import scala.collection.mutable.{ Map => MMap }

trait ClassifierEvaluator[T, L] {
  def apply(classifier: Classifier[T, L]): ClassifierEvaluatorResult[L]
}

class SimpleClassifierEvaluator[T, L](gold: Seq[(L, T)]) extends ClassifierEvaluator[T, L] {
  override def apply(classifier: Classifier[T, L]) = {
    val results = gold.map { case (l, d) => l -> classifier(d) }
    val correct = results.count { case (l, r) => r == l }
    val total = results.size
    val mistakes = results.groupByKey.mapVals(_.counts) // Map[expected, Map[model, count]]
    ClassifierEvaluatorResult(correct, total, mistakes, results)
  }
}

case class ClassifierEvaluatorResult[L](
  correct: Int,
  total: Int,
  mistakes: Map[L, Map[L, Int]], // Map[expected, Map[model, count]]
  results: Seq[(L, L)]) { // Seq[(expected, model)] 

  def accuracyStr = f"accuracy = ${correct.toDouble / total}%.3f ($correct/$total)"

  override def toString = {
    val sb = new StringBuilder
    sb.append(accuracyStr + "\n")
    if (total - correct > 0) {
      sb.append("  mistakes\n")
      sb.append("    gold\tmodel\tcount\n")
      for ((l, (r, c)) <- mistakes.ungroup.toVector.sortBy { case (l, (r, c)) => -c })
        sb.append(s"    $l\t$r\t$c (${c.toDouble / (total - correct)})\n")
    }
    sb.append("  model output dist = %s\n".format(results.map(_._2).counts.normalizeValues))
    sb.result
  }
}
