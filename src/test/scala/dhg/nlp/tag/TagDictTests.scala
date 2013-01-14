package dhg.nlp.tag

import org.junit.Assert._
import org.junit._
import dhg.nlp.test.TestUtils._
import dhg.nlp.tag.TagDict._
import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._

class TagDictTests {

  @Test
  def testSimpleTagDict_noDefault() {
    val td: TagDict[Char, Symbol] = SimpleTagDict(Map('A' -> Set('a, 'b), 'B' -> Set('c)))

    assertEquals(Set('a, 'b, 'c), td.defaultSet)

    assertEquals(Some(Set('a, 'b)), td.doGetSet('A'))
    assertEquals(Some(Set('c)), td.doGetSet('B'))
    assertEquals(None, td.doGetSet('Z'))

    val setIterator: Iterator[(Char, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Set('a, 'b), 'B' -> Set('c)), setIteratorSorted)

    assertEquals(Set('a, 'b), td.set('A'))
    assertEquals(Set('c), td.set('B'))
    assertEquals(Set('a, 'b, 'c), td.set('Z'))

    assertEquals(true, td.contains('A'))
    assertEquals(true, td.contains('B'))
    assertEquals(false, td.contains('Z'))

    assertEquals(true, td.contains('A', 'a))
    assertEquals(true, td.contains('A', 'b))
    assertEquals(false, td.contains('A', 'c))
    assertEquals(false, td.contains('A', 'z))
    assertEquals(false, td.contains('B', 'a))
    assertEquals(false, td.contains('B', 'b))
    assertEquals(true, td.contains('B', 'c))
    assertEquals(false, td.contains('B', 'z))
    assertEquals(false, td.contains('Z', 'a))
    assertEquals(false, td.contains('Z', 'b))
    assertEquals(false, td.contains('Z', 'c))
    assertEquals(false, td.contains('Z', 'z))

    assertEquals(Set('A', 'B'), td.symbols)

    assertEquals(Set('a, 'b, 'c), td.allTags)
  }

  @Test
  def testSimpleTagDict_withDefault() {
    val default = Set('b, 'd)
    val td: TagDict[Char, Symbol] = SimpleTagDict(Map('A' -> Set('a, 'b), 'B' -> Set('c)), default)

    assertEquals(Set('b, 'd), td.defaultSet)

    assertEquals(Some(Set('a, 'b)), td.doGetSet('A'))
    assertEquals(Some(Set('c)), td.doGetSet('B'))
    assertEquals(None, td.doGetSet('Z'))

    val setIterator: Iterator[(Char, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Set('a, 'b), 'B' -> Set('c)), setIteratorSorted)

    assertEquals(Set('a, 'b), td.set('A'))
    assertEquals(Set('c), td.set('B'))
    assertEquals(Set('b, 'd), td.set('Z'))

    assertEquals(true, td.contains('A'))
    assertEquals(true, td.contains('B'))
    assertEquals(false, td.contains('Z'))

    assertEquals(true, td.contains('A', 'a))
    assertEquals(true, td.contains('A', 'b))
    assertEquals(false, td.contains('A', 'c))
    assertEquals(false, td.contains('A', 'd))
    assertEquals(false, td.contains('A', 'z))
    assertEquals(false, td.contains('B', 'a))
    assertEquals(false, td.contains('B', 'b))
    assertEquals(true, td.contains('B', 'c))
    assertEquals(false, td.contains('B', 'd))
    assertEquals(false, td.contains('B', 'z))
    assertEquals(false, td.contains('Z', 'a))
    assertEquals(false, td.contains('Z', 'b))
    assertEquals(false, td.contains('Z', 'c))
    assertEquals(false, td.contains('Z', 'd))
    assertEquals(false, td.contains('Z', 'z))

    assertEquals(Set('A', 'B'), td.symbols)

    assertEquals(Set('a, 'b, 'c, 'd), td.allTags)
  }

  @Test
  def testSimpleWeightedTagDict_withDefault() {
    val default = Map('b -> 4.0, 'd -> 5.0)
    val td: WeightedTagDict[Char, Symbol] = SimpleWeightedTagDict(Map(
      'A' -> Map('a -> 1, 'b -> 2), 'B' -> Map('c -> 3)),
      default)

    // TagDict methods

    assertEquals(Set('b, 'd), td.defaultSet)

    assertEquals(Some(Set('a, 'b)), td.doGetSet('A'))
    assertEquals(Some(Set('c)), td.doGetSet('B'))
    assertEquals(None, td.doGetSet('Z'))

    val setIterator: Iterator[(Char, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Set('a, 'b), 'B' -> Set('c)), setIteratorSorted)

    assertEquals(Set('a, 'b), td.set('A'))
    assertEquals(Set('c), td.set('B'))
    assertEquals(Set('b, 'd), td.set('Z'))

    assertEquals(true, td.contains('A'))
    assertEquals(true, td.contains('B'))
    assertEquals(false, td.contains('Z'))

    assertEquals(true, td.contains('A', 'a))
    assertEquals(true, td.contains('A', 'b))
    assertEquals(false, td.contains('A', 'c))
    assertEquals(false, td.contains('A', 'd))
    assertEquals(false, td.contains('A', 'z))
    assertEquals(false, td.contains('B', 'a))
    assertEquals(false, td.contains('B', 'b))
    assertEquals(true, td.contains('B', 'c))
    assertEquals(false, td.contains('B', 'd))
    assertEquals(false, td.contains('B', 'z))
    assertEquals(false, td.contains('Z', 'a))
    assertEquals(false, td.contains('Z', 'b))
    assertEquals(false, td.contains('Z', 'c))
    assertEquals(false, td.contains('Z', 'd))
    assertEquals(false, td.contains('Z', 'z))

    assertEquals(Set('A', 'B'), td.symbols)

    assertEquals(Set('a, 'b, 'c, 'd), td.allTags)

    // WeightedTagDict methods

    assertEquals(Map('b -> 4.0, 'd -> 5.0), td.default)

    assertEquals(Some(Map('a -> 1.0, 'b -> 2.0)), td.doGetMap('A'))
    assertEquals(Some(Map('c -> 3.0)), td.doGetMap('B'))
    assertEquals(None, td.doGetMap('Z'))

    val iterator: Iterator[(Char, Map[Symbol, Double])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Map('a -> 1.0, 'b -> 2.0), 'B' -> Map('c -> 3.0)), iteratorSorted)

    assertEquals(Map('a -> 1.0, 'b -> 2.0), td.weights('A'))
    assertEquals(Map('c -> 3.0), td.weights('B'))
    assertEquals(Map('b -> 4.0, 'd -> 5.0), td.weights('Z'))
  }

  @Test
  def testUniformWeightedTagDict_noDefault() {
    val td: WeightedTagDict[Char, Symbol] = UniformWeightedTagDict(Map('A' -> Set('a, 'b), 'B' -> Set('c)))

    // TagDict methods

    assertEquals(Set('a, 'b, 'c), td.defaultSet)

    assertEquals(Some(Set('a, 'b)), td.doGetSet('A'))
    assertEquals(Some(Set('c)), td.doGetSet('B'))
    assertEquals(None, td.doGetSet('Z'))

    val setIterator: Iterator[(Char, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Set('a, 'b), 'B' -> Set('c)), setIteratorSorted)

    assertEquals(Set('a, 'b), td.set('A'))
    assertEquals(Set('c), td.set('B'))
    assertEquals(Set('a, 'b, 'c), td.set('Z'))

    assertEquals(true, td.contains('A'))
    assertEquals(true, td.contains('B'))
    assertEquals(false, td.contains('Z'))

    assertEquals(true, td.contains('A', 'a))
    assertEquals(true, td.contains('A', 'b))
    assertEquals(false, td.contains('A', 'c))
    assertEquals(false, td.contains('A', 'z))
    assertEquals(false, td.contains('B', 'a))
    assertEquals(false, td.contains('B', 'b))
    assertEquals(true, td.contains('B', 'c))
    assertEquals(false, td.contains('B', 'z))
    assertEquals(false, td.contains('Z', 'a))
    assertEquals(false, td.contains('Z', 'b))
    assertEquals(false, td.contains('Z', 'c))
    assertEquals(false, td.contains('Z', 'z))

    assertEquals(Set('A', 'B'), td.symbols)

    assertEquals(Set('a, 'b, 'c), td.allTags)

    // WeightedTagDict methods

    assertEquals(Map('a -> 1.0, 'b -> 1.0, 'c -> 1.0), td.default)

    assertEquals(Some(Map('a -> 1.0, 'b -> 1.0)), td.doGetMap('A'))
    assertEquals(Some(Map('c -> 1.0)), td.doGetMap('B'))
    assertEquals(None, td.doGetMap('Z'))

    val iterator: Iterator[(Char, Map[Symbol, Double])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Map('a -> 1.0, 'b -> 1.0), 'B' -> Map('c -> 1.0)), iteratorSorted)

    assertEquals(Map('a -> 1.0, 'b -> 1.0), td.weights('A'))
    assertEquals(Map('c -> 1.0), td.weights('B'))
    assertEquals(Map('a -> 1.0, 'b -> 1.0, 'c -> 1.0), td.weights('Z'))
  }

  @Test
  def testUniformWeightedTagDict_withDefault() {
    val default = Set('b, 'd)
    val td: WeightedTagDict[Char, Symbol] = UniformWeightedTagDict(Map('A' -> Set('a, 'b), 'B' -> Set('c)), default)

    // TagDict methods

    assertEquals(Set('b, 'd), td.defaultSet)

    assertEquals(Some(Set('a, 'b)), td.doGetSet('A'))
    assertEquals(Some(Set('c)), td.doGetSet('B'))
    assertEquals(None, td.doGetSet('Z'))

    val setIterator: Iterator[(Char, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Set('a, 'b), 'B' -> Set('c)), setIteratorSorted)

    assertEquals(Set('a, 'b), td.set('A'))
    assertEquals(Set('c), td.set('B'))
    assertEquals(Set('b, 'd), td.set('Z'))

    assertEquals(true, td.contains('A'))
    assertEquals(true, td.contains('B'))
    assertEquals(false, td.contains('Z'))

    assertEquals(true, td.contains('A', 'a))
    assertEquals(true, td.contains('A', 'b))
    assertEquals(false, td.contains('A', 'c))
    assertEquals(false, td.contains('A', 'd))
    assertEquals(false, td.contains('A', 'z))
    assertEquals(false, td.contains('B', 'a))
    assertEquals(false, td.contains('B', 'b))
    assertEquals(true, td.contains('B', 'c))
    assertEquals(false, td.contains('B', 'd))
    assertEquals(false, td.contains('B', 'z))
    assertEquals(false, td.contains('Z', 'a))
    assertEquals(false, td.contains('Z', 'b))
    assertEquals(false, td.contains('Z', 'c))
    assertEquals(false, td.contains('Z', 'd))
    assertEquals(false, td.contains('Z', 'z))

    assertEquals(Set('A', 'B'), td.symbols)

    assertEquals(Set('a, 'b, 'c, 'd), td.allTags)

    // WeightedTagDict methods

    assertEquals(Map('b -> 1.0, 'd -> 1.0), td.default)

    assertEquals(Some(Map('a -> 1.0, 'b -> 1.0)), td.doGetMap('A'))
    assertEquals(Some(Map('c -> 1.0)), td.doGetMap('B'))
    assertEquals(None, td.doGetMap('Z'))

    val iterator: Iterator[(Char, Map[Symbol, Double])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Map('a -> 1.0, 'b -> 1.0), 'B' -> Map('c -> 1.0)), iteratorSorted)

    assertEquals(Map('a -> 1.0, 'b -> 1.0), td.weights('A'))
    assertEquals(Map('c -> 1.0), td.weights('B'))
    assertEquals(Map('b -> 1.0, 'd -> 1.0), td.weights('Z'))
  }

  @Test
  def testOptionalTagDict() {
    val default = Map('b -> 4.0, 'd -> 5.0)
    val originalTD: WeightedTagDict[Char, Symbol] = SimpleWeightedTagDict(Map(
      'A' -> Map('a -> 1.0, 'b -> 2.0), 'B' -> Map('c -> 3.0)),
      default)
    val td: OptionalWeightedTagDict[Char, Symbol] = OptionalTagDict(originalTD)

    // TagDict methods

    assertEquals(Set(Some('b), Some('d)), td.defaultSet)

    assertEquals(Some(Set(Some('a), Some('b))), td.doGetSet(Some('A')))
    assertEquals(Some(Set(Some('c))), td.doGetSet(Some('B')))
    assertEquals(None, td.doGetSet(Some('Z')))
    assertEquals(Some(Set(None)), td.doGetSet(None))

    val setIterator: Iterator[(Option[Char], Set[Option[Symbol]])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector(Some('A') -> Set(Some('a), Some('b)), Some('B') -> Set(Some('c))), setIteratorSorted)

    assertEquals(Set(Some('a), Some('b)), td.set(Some('A')))
    assertEquals(Set(Some('c)), td.set(Some('B')))
    assertEquals(Set(Some('b), Some('d)), td.set(Some('Z')))
    assertEquals(Set(None), td.set(None))

    assertEquals(true, td.contains(Some('A')))
    assertEquals(true, td.contains(Some('B')))
    assertEquals(false, td.contains(Some('Z')))
    assertEquals(true, td.contains(None))

    assertEquals(true, td.contains(Some('A'), Some('a)))
    assertEquals(true, td.contains(Some('A'), Some('b)))
    assertEquals(false, td.contains(Some('A'), Some('c)))
    assertEquals(false, td.contains(Some('A'), Some('d)))
    assertEquals(false, td.contains(Some('A'), Some('z)))
    assertEquals(false, td.contains(Some('A'), None))
    assertEquals(false, td.contains(Some('B'), Some('a)))
    assertEquals(false, td.contains(Some('B'), Some('b)))
    assertEquals(true, td.contains(Some('B'), Some('c)))
    assertEquals(false, td.contains(Some('B'), Some('d)))
    assertEquals(false, td.contains(Some('B'), Some('z)))
    assertEquals(false, td.contains(Some('B'), None))
    assertEquals(false, td.contains(Some('Z'), Some('a)))
    assertEquals(false, td.contains(Some('Z'), Some('b)))
    assertEquals(false, td.contains(Some('Z'), Some('c)))
    assertEquals(false, td.contains(Some('Z'), Some('d)))
    assertEquals(false, td.contains(Some('Z'), Some('z)))
    assertEquals(false, td.contains(Some('Z'), None))
    assertEquals(false, td.contains(None, Some('a)))
    assertEquals(false, td.contains(None, Some('b)))
    assertEquals(false, td.contains(None, Some('c)))
    assertEquals(false, td.contains(None, Some('d)))
    assertEquals(false, td.contains(None, Some('z)))
    assertEquals(true, td.contains(None, None))

    assertEquals(Set(Some('A'), Some('B')), td.symbols)

    assertEquals(Set(Some('a), Some('b), Some('c), Some('d)), td.allTags)

    // WeightedTagDict methods

    assertEquals(Map(Some('b) -> 4.0, Some('d) -> 5.0), td.default)

    assertEquals(Some(Map(Some('a) -> 1.0, Some('b) -> 2.0)), td.doGetMap(Some('A')))
    assertEquals(Some(Map(Some('c) -> 3.0)), td.doGetMap(Some('B')))
    assertEquals(None, td.doGetMap(Some('Z')))
    assertEquals(Some(Map(None -> 1.0)), td.doGetMap(None))

    val iterator: Iterator[(Option[Char], Map[Option[Symbol], Double])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEquals(Vector(Some('A') -> Map(Some('a) -> 1.0, Some('b) -> 2.0), Some('B') -> Map(Some('c) -> 3.0)), iteratorSorted)

    assertEquals(Map(Some('a) -> 1.0, Some('b) -> 2.0), td.weights(Some('A')))
    assertEquals(Map(Some('c) -> 3.0), td.weights(Some('B')))
    assertEquals(Map(Some('b) -> 4.0, Some('d) -> 5.0), td.weights(Some('Z')))
    assertEquals(Map(None -> 1.0), td.weights(None))
  }

  @Test
  def testNoDefaultTagDict() {
    val default = Map('b -> 4.0, 'd -> 5.0)
    val originalTD: WeightedTagDict[Char, Symbol] = SimpleWeightedTagDict(Map(
      'A' -> Map('a -> 1.0, 'b -> 2.0), 'B' -> Map('c -> 3.0)),
      default)
    val td: WeightedTagDict[Char, Symbol] = NoDefaultTagDict(originalTD)

    // TagDict methods

    assertEquals(Set[Char](), td.defaultSet)

    assertEquals(Some(Set('a, 'b)), td.doGetSet('A'))
    assertEquals(Some(Set('c)), td.doGetSet('B'))
    assertEquals(None, td.doGetSet('Z'))

    val setIterator: Iterator[(Char, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Set('a, 'b), 'B' -> Set('c)), setIteratorSorted)

    assertEquals(Set('a, 'b), td.set('A'))
    assertEquals(Set('c), td.set('B'))
    assertEquals(Set[Char](), td.set('Z'))

    assertEquals(true, td.contains('A'))
    assertEquals(true, td.contains('B'))
    assertEquals(false, td.contains('Z'))

    assertEquals(true, td.contains('A', 'a))
    assertEquals(true, td.contains('A', 'b))
    assertEquals(false, td.contains('A', 'c))
    assertEquals(false, td.contains('A', 'd))
    assertEquals(false, td.contains('A', 'z))
    assertEquals(false, td.contains('B', 'a))
    assertEquals(false, td.contains('B', 'b))
    assertEquals(true, td.contains('B', 'c))
    assertEquals(false, td.contains('B', 'd))
    assertEquals(false, td.contains('B', 'z))
    assertEquals(false, td.contains('Z', 'a))
    assertEquals(false, td.contains('Z', 'b))
    assertEquals(false, td.contains('Z', 'c))
    assertEquals(false, td.contains('Z', 'd))
    assertEquals(false, td.contains('Z', 'z))

    assertEquals(Set('A', 'B'), td.symbols)

    assertEquals(Set('a, 'b, 'c), td.allTags)

    // WeightedTagDict methods

    assertEquals(Map[Char, Symbol](), td.default)

    assertEquals(Some(Map('a -> 1.0, 'b -> 2.0)), td.doGetMap('A'))
    assertEquals(Some(Map('c -> 3.0)), td.doGetMap('B'))
    assertEquals(None, td.doGetMap('Z'))

    val iterator: Iterator[(Char, Map[Symbol, Double])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEquals(Vector('A' -> Map('a -> 1.0, 'b -> 2.0), 'B' -> Map('c -> 3.0)), iteratorSorted)

    assertEquals(Map('a -> 1.0, 'b -> 2.0), td.weights('A'))
    assertEquals(Map('c -> 3.0), td.weights('B'))
    assertEquals(Map[Char, Symbol](), td.weights('Z'))
  }

}
