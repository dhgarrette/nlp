package dhg.nlp.tag.support

import org.junit.Assert._
import org.junit.Test

import dhg.nlp.tag._
import dhg.nlp.util.CollectionUtils._
import dhg.nlp.test.TestUtils._
import dhg.util.CollectionUtil._
import dhg.nlp.freq._

class TagDictFactoryTests {

  @Test
  def test_SimpleWeightedTagDictFactory_passthroughTransformer() {
    val td: WeightedTagDict[String, Symbol] =
      new SimpleWeightedTagDictFactory[String, Symbol](
        PassthroughCondCountsTransformer())
        .make(Seq(Vector(
          ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D),
          ("dog", 'N), ("dog", 'N), ("dog", 'N),
          ("walks", 'V), ("walks", 'V), ("walks", 'V), ("walks", 'N))))

    // TagDict methods

    assertEquals(Set('D, 'N, 'V), td.defaultSet)

    assertEquals(Some(Set('D)), td.doGetSet("the"))
    assertEquals(Some(Set('N)), td.doGetSet("dog"))
    assertEquals(Some(Set('V, 'N)), td.doGetSet("walks"))
    assertEquals(None, td.doGetSet("aardvark"))

    val setIterator: Iterator[(String, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector("dog" -> Set('N), "the" -> Set('D), "walks" -> Set('V, 'N)), setIteratorSorted)

    assertEquals(Set('D), td.set("the"))
    assertEquals(Set('N), td.set("dog"))
    assertEquals(Set('V, 'N), td.set("walks"))
    assertEquals(Set('D, 'N, 'V), td.set("aardvark"))

    assertEquals(true, td.contains("the"))
    assertEquals(true, td.contains("dog"))
    assertEquals(true, td.contains("walks"))
    assertEquals(false, td.contains("aardvark"))

    assertEquals(true, td.contains("the", 'D))
    assertEquals(false, td.contains("the", 'N))
    assertEquals(false, td.contains("the", 'V))
    assertEquals(false, td.contains("the", 'Z))

    assertEquals(false, td.contains("dog", 'D))
    assertEquals(true, td.contains("dog", 'N))
    assertEquals(false, td.contains("dog", 'V))
    assertEquals(false, td.contains("dog", 'Z))

    assertEquals(false, td.contains("walks", 'D))
    assertEquals(true, td.contains("walks", 'N))
    assertEquals(true, td.contains("walks", 'V))
    assertEquals(false, td.contains("walks", 'Z))

    assertEquals(false, td.contains("aardvark", 'D))
    assertEquals(false, td.contains("aardvark", 'N))
    assertEquals(false, td.contains("aardvark", 'V))
    assertEquals(false, td.contains("aardvark", 'Z))

    assertEquals(Set("the", "dog", "walks"), td.symbols)

    assertEquals(Set('D, 'N, 'V), td.allTags)

    // WeightedTagDict methods

    assertEqualsSmart(Map('D -> (5 / 12.0), 'N -> (4 / 12.0), 'V -> (3 / 12.0)), td.default)

    assertEqualsSmart(Some(Map('D -> (5 / 5.0))), td.doGetMap("the"))
    assertEqualsSmart(Some(Map('N -> (3 / 3.0))), td.doGetMap("dog"))
    assertEqualsSmart(Some(Map('V -> (3 / 4.0), 'N -> (1 / 4.0))), td.doGetMap("walks"))
    assertEqualsSmart(None, td.doGetMap("aardvark"))

    val iterator: Iterator[(String, Map[Symbol, Double])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEqualsSmart(Vector("dog" -> Map('N -> (3 / 3.0)), "the" -> Map('D -> (5 / 5.0)), "walks" -> Map('V -> (3 / 4.0), 'N -> (1 / 4.0))), iteratorSorted)

    assertEqualsSmart(Map('D -> (5 / 5.0)), td.weights("the"))
    assertEqualsSmart(Map('N -> (3 / 3.0)), td.weights("dog"))
    assertEqualsSmart(Map('V -> (3 / 4.0), 'N -> (1 / 4.0)), td.weights("walks"))
    assertEqualsSmart(Map('D -> (5 / 12.0), 'N -> (4 / 12.0), 'V -> (3 / 12.0)), td.weights("aardvark"))
  }

  @Test
  def test_SimpleWeightedTagDictFactory_addOneSmoothingTransformer() {
    val td: WeightedTagDict[String, Symbol] =
      new SimpleWeightedTagDictFactory[String, Symbol](
        AddLambdaSmoothingCondCountsTransformer(1))
        .make(Seq(Vector(
          ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D),
          ("dog", 'N), ("dog", 'N), ("dog", 'N),
          ("walks", 'V), ("walks", 'V), ("walks", 'V), ("walks", 'N))))

    // TagDict methods

    assertEquals(Set('D, 'N, 'V), td.defaultSet)

    assertEquals(Some(Set('D, 'N, 'V)), td.doGetSet("the"))
    assertEquals(Some(Set('D, 'N, 'V)), td.doGetSet("dog"))
    assertEquals(Some(Set('D, 'N, 'V)), td.doGetSet("walks"))
    assertEquals(None, td.doGetSet("aardvark"))

    val setIterator: Iterator[(String, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector("dog" -> Set('D, 'N, 'V), "the" -> Set('D, 'N, 'V), "walks" -> Set('D, 'N, 'V)), setIteratorSorted)

    assertEquals(Set('D, 'N, 'V), td.set("the"))
    assertEquals(Set('D, 'N, 'V), td.set("dog"))
    assertEquals(Set('D, 'N, 'V), td.set("walks"))
    assertEquals(Set('D, 'N, 'V), td.set("aardvark"))

    assertEquals(true, td.contains("the"))
    assertEquals(true, td.contains("dog"))
    assertEquals(true, td.contains("walks"))
    assertEquals(false, td.contains("aardvark"))

    assertEquals(true, td.contains("the", 'D))
    assertEquals(true, td.contains("the", 'N))
    assertEquals(true, td.contains("the", 'V))
    assertEquals(false, td.contains("the", 'Z))

    assertEquals(true, td.contains("dog", 'D))
    assertEquals(true, td.contains("dog", 'N))
    assertEquals(true, td.contains("dog", 'V))
    assertEquals(false, td.contains("dog", 'Z))

    assertEquals(true, td.contains("walks", 'D))
    assertEquals(true, td.contains("walks", 'N))
    assertEquals(true, td.contains("walks", 'V))
    assertEquals(false, td.contains("walks", 'Z))

    assertEquals(false, td.contains("aardvark", 'D))
    assertEquals(false, td.contains("aardvark", 'N))
    assertEquals(false, td.contains("aardvark", 'V))
    assertEquals(false, td.contains("aardvark", 'Z))

    assertEquals(Set("the", "dog", "walks"), td.symbols)

    assertEquals(Set('D, 'N, 'V), td.allTags)

    // WeightedTagDict methods

    assertEqualsSmart(Map('D -> (8 / 24.0), 'N -> (7 / 24.0), 'V -> (6 / 24.0)), td.default)

    assertEqualsSmart(Some(Map('D -> (6 / 9.0), 'N -> (1 / 9.0), 'V -> (1 / 9.0))), td.doGetMap("the"))
    assertEqualsSmart(Some(Map('D -> (1 / 7.0), 'N -> (4 / 7.0), 'V -> (1 / 7.0))), td.doGetMap("dog"))
    assertEqualsSmart(Some(Map('D -> (1 / 8.0), 'N -> (2 / 8.0), 'V -> (4 / 8.0))), td.doGetMap("walks"))
    assertEqualsSmart(None, td.doGetMap("aardvark"))

    val iterator: Iterator[(String, Map[Symbol, Double])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEqualsSmart(Vector(
      "dog" -> Map('D -> (1 / 7.0), 'N -> (4 / 7.0), 'V -> (1 / 7.0)),
      "the" -> Map('D -> (6 / 9.0), 'N -> (1 / 9.0), 'V -> (1 / 9.0)),
      "walks" -> Map('D -> (1 / 8.0), 'N -> (2 / 8.0), 'V -> (4 / 8.0))), iteratorSorted)

    assertEqualsSmart(Map('D -> (6 / 9.0), 'N -> (1 / 9.0), 'V -> (1 / 9.0)), td.weights("the"))
    assertEqualsSmart(Map('D -> (1 / 7.0), 'N -> (4 / 7.0), 'V -> (1 / 7.0)), td.weights("dog"))
    assertEqualsSmart(Map('D -> (1 / 8.0), 'N -> (2 / 8.0), 'V -> (4 / 8.0)), td.weights("walks"))
    assertEqualsSmart(Map('D -> (8 / 24.0), 'N -> (7 / 24.0), 'V -> (6 / 24.0)), td.weights("aardvark"))
  }

  def assertEqualsSmart[A](expected: Map[A, Double], actual: Map[A, Double]) {
    assertEquals(expected.keys.toSet, actual.keys.toSet)
    for (k <- expected.keys) assertEqualsProb(expected(k), actual(k))
  }

  def assertEqualsSmart[A](expected: Option[Map[A, Double]], actual: Option[Map[A, Double]]) {
    assertEquals(expected.isDefined, actual.isDefined)
    if (expected.isDefined) assertEqualsSmart(expected.get, actual.get)
  }

  def assertEqualsSmart[A, B](expected: Vector[(A, Map[B, Double])], actual: Vector[(A, Map[B, Double])]) {
    assertEquals(expected.size, actual.size)
    for (((eA, eB), (aA, aB)) <- expected zip actual) {
      assertEquals(eA, aA)
      assertEqualsSmart(eB, aB)
    }
  }
}
