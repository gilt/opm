package com.gilt.opm

import java.util.UUID
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.mongodb.casbah.MongoConnection

/**
 * Exercises various edge cases around the supported collection types (List, Set, Seq, Map, IndexedSeq & Vector).
 *
 * This also shows some inherent limitations currently in how opm encodes collection types.  The short answer is
 * that we do not, which makes it impossible to reconstruct collection types correctly. You'll notice below in
 * testObj that when we load a complex set of nested collections, type information is not correctly stored and
 * the type contract lies. The fix for this is to store information about the collection type along with the
 * collection.
 *
 * @author Eric Bowman
 * @since 2/3/13 7:50 PM
 */
object ComplexCollectionTest {

  trait Nested extends OpmObject {
    def name: String
  }

  trait ComplexCollections extends OpmObject {
    def aList: List[String]

    def aSet: Set[Int]

    def aSeq: Seq[Long]

    def anIndexedSeq: IndexedSeq[Float]

    def aVector: Vector[UUID]

    def aComplexMap: Map[String, Set[Vector[String]]]

    def aComplexMapWithObjs: Map[String, Set[Vector[Nested]]]
  }


}

class ComplexCollectionTest extends FunSuite with OpmMongoStorage[ComplexCollectionTest.ComplexCollections] with CollectionHelper with ShouldMatchers {
  def collectionName = "opm-ComplexCollectionTest"

  import OpmFactory._
  import ComplexCollectionTest.{ComplexCollections, Nested}
  import CollectionHelper.databaseName

  lazy val nestedStorage = Some(new OpmMongoStorage[Nested] {
    val collection = MongoConnection()(databaseName)("opm-ComplexCollectionTest-nested")
    val locks = MongoConnection()(databaseName)("opm-ComplexCollectionTest-nested-locks")
  })

  override def nestedToStorage[T <: OpmObject](obj: Option[T] = None)(implicit mf: Manifest[T]) = nestedStorage.asInstanceOf[Option[OpmStorage[T]]]
  test("arbitrary collections can be stored & retrieved") {
    val obj = instance[ComplexCollections]("a", Map(
      "aList" -> List("1", "2", "3"),
      "aSet" -> Set(1, 2, 3),
      "aSeq" -> Seq(1L, 2L, 3L),
      "anIndexedSeq" -> IndexedSeq(1f, 2f, 3f),
      "aVector" -> Vector(UUID.randomUUID(), UUID.randomUUID()),
      "aComplexMap" -> Map("a" -> Set(Vector("1", "2"), Vector("3", "4"))),
      "aComplexMapWithObjs" -> Map("a" -> Set(Vector(instance[Nested]("aa", Map("name" -> "aa"))), Vector(instance[Nested]("bb", Map("name" -> "bb")))))
    ))

    testObj(obj)

    put(obj)

    val loaded = get("a").get

    testObj(loaded)
  }

  def testObj(obj: ComplexCollections) {
    assert(obj.aList.isInstanceOf[List[_]])
    obj.aList should equal(List("1", "2", "3"))

    assert(obj.aSet.isInstanceOf[Set[_]])
    obj.aSet should equal(Set(1, 2, 3))

    assert(obj.aSeq.isInstanceOf[Seq[_]])
    obj.aSeq should equal(Seq(1L, 2L, 3L))

    assert(obj.anIndexedSeq.isInstanceOf[IndexedSeq[_]])
    obj.anIndexedSeq should equal(IndexedSeq(1f, 2f, 3f))

    assert(obj.aVector.isInstanceOf[Vector[_]])
    obj.aVector.size should equal(2)

    obj.aComplexMap should equal(Map("a" -> Set(Vector("1", "2"), Vector("3", "4"))))
    obj.aComplexMapWithObjs("a").toSeq.flatten.map(_.name).toSet should equal(Set("aa", "bb"))
  }
}
