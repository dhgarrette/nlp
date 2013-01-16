package dhg.nlp.classify

/**
 * @tparam T	The type of the document
 * @tparam L	The type of the label
 */
trait Classifier[T, L] extends (T => L) {

  def apply(document: T): L = classify(document)
  def classify(document: T): L = scores(document).maxBy(_._2)._1

  def scores(document: T): Map[L, Double]

}
