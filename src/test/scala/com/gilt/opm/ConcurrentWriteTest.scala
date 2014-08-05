package com.gilt.opm

import java.util.ConcurrentModificationException

import org.scalatest.{FunSuite, Matchers}

/**
 * Tests the core concurrent modification contract.
 *
 * @author Eric Bowman
 * @since 12/17/12 6:37 PM
 */

object ConcurrentWriteTest {

  trait SimpleObject extends OpmObject {
    def foo: String
  }

}

class ConcurrentWriteTest extends FunSuite with OpmMongoStorage[ConcurrentWriteTest.SimpleObject] with Matchers with CollectionHelper {

  import com.gilt.opm.ConcurrentWriteTest.SimpleObject
  import com.gilt.opm.OpmFactory._

  override val collectionName = "opm-ConcurrentWriteTest"

  test( """Trying to write an object with divergent history throws a ConcurrentModificationException""") {
    val foo = instance[SimpleObject]("a", Map("foo" -> "initial value"))
    put(foo)

    val foo1 = get("a").map(_.set(_.foo).to("new value 1")).get
    val foo2 = get("a").map(_.set(_.foo).to("new value 2")).get

    put(foo1)

    an [ConcurrentModificationException] should be thrownBy put(foo2)
  }

  test( """Trying to squashPut an object with divergent history succeeds""") {
    val foo = instance[SimpleObject]("a", Map("foo" -> "initial value"))
    put(foo)

    val foo1 = get("a").map(_.set(_.foo).to("new value 1")).get
    val foo2 = get("a").map(_.set(_.foo).to("new value 2")).get

    put(foo1)
    squashPut(foo2)
    get("a").get.foo should equal("new value 2")
  }
}
