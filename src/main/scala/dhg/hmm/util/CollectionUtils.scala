package dhg.hmm.util

import scala.collection.GenIterable
import scala.collection.GenIterableLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.IterableLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.util.Random

import dhg.util.CollectionUtil._

object CollectionUtils {

  //////////////////////////////////////////////////////
  // countCompare(p: A => Boolean, count: Int): Int
  //   - Compares the number of items satisfying a predicate to a test value.
  //   - Functionally equivalent to (but more efficient than):
  //         this.count(p).compareTo(count)
  //////////////////////////////////////////////////////

  implicit class Enriched_countCompare_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Compares the number of items satisfying a predicate to a test value.
     *
     *   @param p       the predicate used to test elements.
     *   @param count   the test value that gets compared with the count.
     *   @return Int value `x` where
     *   {{{
     *        x <  0       if this.count(p) <  count
     *        x == 0       if this.count(p) == count
     *        x >  0       if this.count(p) >  count
     *   }}}
     *  The method as implemented here does not call `length` directly; its running time
     *  is `O(length min count)` instead of `O(length)`.
     */
    def countCompare(p: A => Boolean, count: Int): Int = {
      val itr = self.toIterator
      var i = 0
      while (itr.hasNext && i <= count) {
        if (p(itr.next))
          i += 1
      }
      i - count
    }
  }

  //////////////////////////////////////////////////////
  // sumBy[B: Numeric](f: A => B): B
  //   - Map a numeric-producing function over each item and sum the results 
  //   - Functionally equivalent to:
  //         this.map(f).sum
  //////////////////////////////////////////////////////

  implicit class Enriched_sumBy_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Map a numeric-producing function over each item and sum the results.
     *
     * Functionally equivalent to `this.map(f).sum`
     *
     * @param f	A function that produces a Numeric
     * @return the sum of the results after applications of f
     */
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      val itr = self.toIterator
      var accum = num.zero
      while (itr.hasNext)
        accum = num.plus(accum, f(itr.next))
      return accum
    }
  }

  //////////////////////////////////////////////////////
  // sliding2: Iterator[(A,A)]
  //   - slide over this collection to produce pairs.
  //   - Functionally equivalent to:
  //         this.sliding(2).map{Seq(a,b) => (a,b)}
  //////////////////////////////////////////////////////

  implicit class Enriched_sliding2_Iterator[A](self: Iterator[A]) {
    /**
     * Slide over this collection to produce pairs.
     */
    def sliding2[B >: A](): Iterator[(B, B)] =
      self.sliding(2).map(_.toTuple2)
  }

  implicit class Enriched_sliding2_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Slide over this collection to produce pairs.
     */
    def sliding2[B >: A](): Iterator[(B, B)] =
      self.toIterator.sliding2()
  }

  //////////////////////////////////////////////////////
  // groupBy(f: A => K): Repr[(R,U)]
  //   - Make Traversable.groupBy functionality available to Iterator
  //////////////////////////////////////////////////////

  implicit class Enriched_groupBy_Iterator[A](self: Iterator[A]) {
    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @return Map from new keys to original items
     */
    def groupBy[K](f: A => K): Map[K, Vector[A]] =
      this.groupBy(f, Vector.newBuilder[A])

    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @param builder	a builder to construct collections of items that have been grouped
     * @return Map from new keys to original items
     */
    def groupBy[K, That <: Iterable[A]](f: A => K, builder: => Builder[A, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[A, That]]
      for (elem <- self) {
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, builder)
        bldr += elem
      }
      val b = Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // groupByKey(): Map[T,Repr[U]]
  //   - For a collection of pairs, group by the first item in the pair.
  //////////////////////////////////////////////////////

  implicit class Enriched_groupByKey_Iterator[A](self: Iterator[A]) {
    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U](implicit ev: A <:< (T, U)): Map[T, Vector[U]] =
      this.groupByKey(Vector.newBuilder[U])

    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @param builder a builder to construct collections of items that have been grouped
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U, That <: Iterable[U]](builder: => Builder[U, That])(implicit ev: A <:< (T, U)): Map[T, That] =
      self.groupBy(_._1).mapVals(v => (builder ++= v.map(_._2)).result)
  }

  //////////////////////////////////////////////////////
  // +++(other: TraversableOnce[(T,Traversable[S])]): Repr[(T,Traversable[S])]
  //   - Given two collections of pairs (T,Traversable[S]), combine into
  //     a single collection such that all values associated with the same
  //     key are concatenated.
  //   - Functionally equivalent to:
  //         (this.iterator ++ other).groupBy(_._1).mapValues(_.map(_._2).reduce(_ ++ _))
  //////////////////////////////////////////////////////

  implicit class Enriched_$plus$plus$plus_TraversableLike_Traversable[T, S, Repr2 <: Traversable[S], Repr1 <: Traversable[(T, TraversableLike[S, Repr2])]](self: TraversableLike[(T, TraversableLike[S, Repr2]), Repr1]) {
    /**
     * Given two collections of pairs (T,Traversable[S]), combine into
     * a single collection such that all values associated with the same
     * key are concatenated.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def +++[That1 <: Traversable[(T, TraversableLike[S, Repr2])], That2](other: TraversableOnce[(T, TraversableLike[S, Repr2])])(implicit bf2: CanBuildFrom[Repr2, S, That2], bf1: CanBuildFrom[Repr1, (T, That2), That1]) = {
      val grouped = (self.toIterator ++ other).groupByKey
      val b = bf1(grouped.asInstanceOf[Repr1])
      b.sizeHint(grouped.size)
      for ((k, vs) <- grouped) {
        val b2 = bf2()
        for (v <- vs) b2 ++= v
        b += k -> b2.result
      }
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // +++[U:Numeric](other: Traversable[(T,U)]): Repr[(T,U)]
  //   - Given two collections of pairs (T,U:Numeric), combine into
  //     a single collection such that all values associated with the same
  //     key are summed.
  //   - Functionally equivalent to:
  //         (this.iterator ++ other).groupBy(_._1).mapValues(_.map(_._2).sum)
  //////////////////////////////////////////////////////

  implicit class Enriched_$plus$plus$plus_TraversableLike_Numeric[T, U: Numeric, Repr <: Traversable[(T, U)]](self: TraversableLike[(T, U), Repr]) {
    /**
     * Given two collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def +++[That <: Traversable[(T, U)]](other: TraversableOnce[(T, U)])(implicit bf: CanBuildFrom[Repr, (T, U), That]) = {
      val grouped = (self.toIterator ++ other).groupByKey
      val b = bf(grouped.asInstanceOf[Repr])
      b.sizeHint(grouped.size)
      for ((k, vs) <- grouped) b += k -> vs.sum
      b.result
    }
  }

  //  implicit class Enriched_$plus$plus$plus_Iterator[T, U](self: Iterator[(T, U)]) {
  //    /**
  //     * In a collection of pairs, map a function over the second item of each
  //     * pair.
  //     *
  //     * @param f	function to map over the second item of each pair
  //     * @return a collection of pairs
  //     */
  //    def +++[R] = self.mapValues(f)
  //  }

  //////////////////////////////////////////////////////
  // flattenByKey: Repr[(T,Traversable[S])]
  //   - Reduce a collection of collections of pairs (T,Traversable[S]), into
  //     a single collection such that all values associated with the same
  //     key are concatenated.
  //   - Functionally equivalent to:
  //         this.flatten.groupBy(_._1).mapValues(_.map(_._2).reduce(_ ++ _))
  //       or
  //         this.reduce(_ +++ _)
  //////////////////////////////////////////////////////

  implicit class Enriched_flattenByKey_TraversableOnce_Traversable[T, S, Repr2 <: Traversable[S], Repr1 <: Traversable[(T, TraversableLike[S, Repr2])]](self: TraversableOnce[TraversableLike[(T, TraversableLike[S, Repr2]), Repr1]]) {
    /**
     * Reduce a collection of collections of pairs (T,Traversable[S]), into
     * a single collection such that all values associated with the same
     * key are concatenated.
     *
     * @return a collection of pairs
     */
    def flattenByKey[That1 <: Traversable[(T, TraversableLike[S, Repr2])], That2](implicit bf2: CanBuildFrom[Repr2, S, That2], bf1: CanBuildFrom[Repr1, (T, That2), That1]) = {
      val grouped = self.toIterator.flatten.groupByKey
      val b = bf1(grouped.asInstanceOf[Repr1])
      for ((k, vs) <- grouped) {
        val b2 = bf2()
        for (v <- vs) b2 ++= v
        b += k -> b2.result
      }
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // sumByKey[U:Numeric](other: Traversable[(T,U)]): Repr[(T,U)]
  //   - Given a collection of collections of pairs (T,U:Numeric), combine into
  //     a single collection such that all values associated with the same
  //     key are summed.
  //   - Functionally equivalent to:
  //         (this.iterator ++ other).groupBy(_._1).mapValues(_.map(_._2).sum)
  //       or
  //         (this.iterator ++ other).groupByKey.mapValues(_.sum)
  //       or
  //         this.reduce(_ +++ _)
  //////////////////////////////////////////////////////

  implicit class Enriched_sumByKey_GenTraversableOnce_Numeric[T, U: Numeric, Repr <: Traversable[(T, U)]](self: GenTraversableOnce[TraversableLike[(T, U), Repr]]) {
    /**
     * Given a collection of collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def sumByKey[That <: Traversable[(T, U)]](implicit bf: CanBuildFrom[Repr, (T, U), That]) = {
      val grouped = self.toIterator.flatten.groupByKey
      val b = bf(grouped.asInstanceOf[Repr])
      b.sizeHint(grouped.size)
      for ((k, vs) <- grouped) b += k -> vs.sum
      b.result
    }
  }

  implicit class Enriched_sumByKey_GenTraversableOnce_Iterator[T, U: Numeric](self: GenTraversableOnce[Iterator[(T, U)]]) {
    /**
     * Given a collection of collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def sumByKey = self.toIterator.flatten.groupByKey.mapVals(_.sum)
  }

  //////////////////////////////////////////////////////
  // takeSub[GenIterable[B]](n: Int): Repr[GenIterable[B]]
  //   - Take iterables from this collection until the total number of 
  //     elements in the taken items is about to exceed `n`.  The total number
  //     of elements will be less than or equal to `n`.
  //////////////////////////////////////////////////////

  implicit class Enriched_takeSub_Iterator[A, R <: GenIterable[A]](self: Iterator[GenIterableLike[A, R]]) {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub(n: Int): Iterator[R] = {
      if (self.isEmpty) {
        self.asInstanceOf[Iterator[R]]
      }
      else {
        new Iterator[R] {
          private var nextElement: R = self.next.asInstanceOf[R]
          private var total: Int = nextElement.size

          override def hasNext = total <= n

          override def next = {
            if (hasNext) {
              val x = nextElement
              if (self.hasNext) {
                nextElement = self.next.asInstanceOf[R]
                total += nextElement.size
              }
              else
                total = n + 1
              x
            }
            else
              throw new NoSuchElementException("next on empty iterator")
          }
        }
      }
    }
  }

  implicit class Enriched_takeSub_GenTraversableLike[A, R <: GenIterable[A], Repr <: GenTraversable[GenIterable[A]]](self: GenTraversableLike[GenIterableLike[A, R], Repr]) {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub[That](n: Int)(implicit bf: CanBuildFrom[Repr, R, That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      for (x <- self.toIterator.takeSub(n)) b += x
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // shuffle
  //////////////////////////////////////////////////////

  implicit class Enriched_shuffle_GenTraversableOnce[T, CC[X] <: TraversableOnce[X]](xs: CC[T]) {
    def shuffle(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
      bf(xs) ++= Random.shuffle(xs) result
    }
  }

  //  implicit class ReversableIterableMap[A, B](map: Map[A, GenTraversableOnce[B]]) {
  //    def reverse(): Map[B, GenTraversableOnce[A]] =
  //      map.ungroup.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }
  //
  //  implicit class ReversableMap[A, B](map: Map[A, B]) {
  //    def reverse(): Map[B, Iterable[A]] =
  //      map.toIndexedSeq.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }

  /**
   * This Set implementation always returns 'true' from its 'contains' method.
   */
  class UniversalSet[A] extends Set[A] {
    override def contains(key: A): Boolean = true
    override def iterator: Iterator[A] = sys.error("not implemented")
    override def +(elem: A): UniversalSet[A] = sys.error("not implemented")
    override def -(elem: A): UniversalSet[A] = sys.error("not implemented")
    override def toString() = "UniversalSet()"
  }
  object UniversalSet {
    def apply[A]() = new UniversalSet[A]
  }

  //////////////////////////////////////////////////////
  // Conversion (.toX) methods
  //////////////////////////////////////////////////////
  implicit class EnrichedWithToMMap[K, V](self: TraversableOnce[(K, V)]) {
    def toMMap =
      if (self.isEmpty) mutable.Map.empty[K, V]
      else mutable.Map.newBuilder[K, V] ++= self result
  }
}
