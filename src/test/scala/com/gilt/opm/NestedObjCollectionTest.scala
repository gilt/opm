package com.gilt.opm

import org.scalatest.FunSuite
import com.mongodb.casbah.MongoConnection

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 2/3/13 5:08 PM
 */
object NestedObjTests {

  trait Nested extends OpmObject {
    def name: String
  }

  trait NestedObjObject extends OpmObject {
    def objs: List[Nested]
  }
}

class NestedObjTests extends FunSuite with OpmMongoStorage[NestedObjTests.NestedObjObject] with CollectionHelper {
  def collectionName = "opm_NestedObjTests"

  import CollectionHelper.databaseName
  import NestedObjTests._
  import OpmFactory._

  lazy val nestedStorage = Some(new OpmMongoStorage[Nested] {
      val collection = MongoConnection()(databaseName)("opm-NestedObjTests-nested")
      val locks = MongoConnection()(databaseName)("opm-NestedObjTests-nested-locks")
    })

  override def nestedToStorage[T <: OpmObject](obj: Option[T] = None)(implicit mf: Manifest[T]) = nestedStorage.asInstanceOf[Option[OpmStorage[T]]]

  test("collection of nested objects can be stored & retrieved") {
    val obj = instance[NestedObjObject]("a", Map("objs" -> List(instance[Nested]("b", Map("name" -> "b")))))
    assert(obj.objs.head.name === "b")
    put(obj)
    val loaded = get("a").get
    assert(loaded.objs.head.name === "b")
  }
}
