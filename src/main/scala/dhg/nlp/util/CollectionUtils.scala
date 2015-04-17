package dhg.nlp.util

import scala.collection.GenIterable
import scala.collection.GenIterableLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

import dhg.util._

object CollectionUtils {

  //  implicit class ReversableIterableMap[A, B](val map: Map[A, GenTraversableOnce[B]]) extends AnyVal {
  //    def reverse(): Map[B, GenTraversableOnce[A]] =
  //      map.ungroup.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }
  //
  //  implicit class ReversableMap[A, B](val map: Map[A, B]) extends AnyVal {
  //    def reverse(): Map[B, Iterable[A]] =
  //      map.toIndexedSeq.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }

  //////////////////////////////////////////////////////
  // Conversion (.toX) methods
  //////////////////////////////////////////////////////
  implicit class EnrichedWithToMMap[K, V](val self: TraversableOnce[(K, V)]) extends AnyVal {
    def toMMap =
      if (self.isEmpty) mutable.Map.empty[K, V]
      else mutable.Map.newBuilder[K, V] ++= self result
  }
}
