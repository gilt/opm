package com.gilt.opm

import org.scalatest.FunSuite
import com.gilt.opm.ListTests.ListObject

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 2/3/13 5:08 PM
 */
object ListTests {

  trait ListObject extends OpmObject {
    def list: List[String]
  }

}

class ListTests extends FunSuite with OpmMongoStorage[ListTests.ListObject] with CollectionHelper {
  def collectionName = "opm-ListTests"

  import ListTests.ListObject
  import OpmFactory._

  test("simple set object can be stored & retrieved") {
    val setObj = instance[ListObject]("b", Map("list" -> List.empty))
    val obj = setObj.set(_.list) := List("a", "b", "c")

    put(obj)

    val loaded = get("b").get
    assert(loaded.list === List("a", "b", "c"))
  }

}
