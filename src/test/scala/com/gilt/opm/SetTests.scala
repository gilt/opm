package com.gilt.opm

import org.scalatest.FunSuite
import com.gilt.opm.SetTests.SetObject

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 2/3/13 5:08 PM
 */
object SetTests {

  trait SetObject extends OpmObject {
    def aSet: Set[String]       // subtle: not allowed to be named set because of .set(...).to(...) magic
  }

}

class SetTests extends FunSuite with OpmMongoStorage[SetTests.SetObject] with CollectionHelper {
  def collectionName = "opm-SetTests"

  import SetTests.SetObject
  import OpmFactory._

  test("simple set object can be stored & retrieved") {
    val setObj = instance[SetObject]("b", Map("aSet" -> Set.empty))
    val obj = setObj.set(_.aSet) := Set("a", "b", "c")

    put(obj)

    val loaded = get("b").get
    assert(loaded.aSet === Set("c", "b", "a"))

  }

}
