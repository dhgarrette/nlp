package dhg.nlp.tag

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

object TagUtils {

  implicit class Enriched_ended_Seq[A, Repr](self: SeqLike[A, Repr]) {
    def ended[That](implicit bf: CanBuildFrom[Repr, Option[A], That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      b += None
      for (x <- self) b += Some(x)
      b += None
      b.result
    }
  }

}
