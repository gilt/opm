package com.gilt.opm

import com.gilt.opm.MixinBuilderTest.Mixins
import org.scalatest.{FunSuite, Matchers}

/**
 * An interesting use case for opm is for a general-purpose builder object.
 * The copy method on a case class is really nice, but it's not so nice to
 * have to be tied to an implementation. We noticed that OPM makes it easy
 * to create a builder or blackboard object from a trait. This test checks
 * that that keeps working, and also that you can mixin several interfaces
 * and create a builder for them.
 *
 * @author Eric Bowman
 * @since 2/9/13 11:36 AM
 */
object MixinBuilderTest {

  trait Behavior1 {
    def name1: String
  }

  trait Behavior2 {
    def name2: String
  }

  trait Mixins extends Behavior1 with Behavior2 with OpmObject {
    def opt: Option[String]
  }

}

class MixinBuilderTest extends FunSuite with Matchers {

  import com.gilt.opm.OpmFactory._

  test("Mixin should opm-ize correctly") {
    val obj = instance[MixinBuilderTest.Mixins]()
    val newObj = obj.set(_.name1).to("1").set(_.name2).to("2").prune
    newObj.name1 should equal("1")
    newObj.name2 should equal("2")
  }

  test("isComplete works") {
    var obj: Mixins = instance[MixinBuilderTest.Mixins]()
    obj.opmIsComplete should equal(false)
    obj = obj.set(_.name1).to("1")
    obj.opmIsComplete should equal(false)
    obj = obj.set(_.name2).to("2")
    obj.opmIsComplete should equal(false)
    obj = obj.set(_.opt).to(Some("3"))
    obj.opmIsComplete should equal(true)
  }
}
