package com.gilt.opm

import org.scalatest.FunSuite
import com.gilt.opm.SeqTests.SeqObject

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 2/3/13 5:08 PM
 */
object SeqTests {

  trait SeqObject extends OpmObject {
    def seq: Seq[String]
  }

}

class SeqTests extends FunSuite with OpmMongoStorage[SeqTests.SeqObject] with CollectionHelper {
  def collectionName = "opm_SeqTests"

  import SeqTests.SeqObject
  import OpmFactory._

  test("simple set object can be stored & retrieved") {
    val setObj = instance[SeqObject]("b", Map("seq" -> Seq.empty))
    val obj = setObj.set(_.seq) := List("a", "b", "c")

    put(obj)

    val loaded = get("b").get
    assert(loaded.seq === Seq("a", "b", "c"))
  }

}
