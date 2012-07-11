package com.gilt.opm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 7/9/12 1:58 PM
 */

object OpmTest {

  trait Foo extends OpmObject {
    def id: Long

    def name: String

    def bar: Bar
  }

  trait Bar extends OpmObject {
    def name: String

    def things: Set[String]
  }
}


class OpmTest extends FunSuite with ShouldMatchers with OpmFactory {

  import OpmTest.{Foo, Bar}

  var time = 0l
  override def clock() = {
    val tmp = time
    time += 1
    tmp
  }

  test("basics") {

    val foo = instance[Foo]

    val foo2 = foo.set(_.name) := "test"

    assert(foo2.name === "test")

    val foo3 = foo2.set(_.bar).to(instance[Bar])
    val foo4 = foo3.set(_.bar.name).to("a bar")
    assert(foo4.bar.name === "a bar")

    val foo5 = foo4.set(_.bar.things) to (Set("a,b,c".split(","): _*))
    assert(foo5.bar.things === Set("a", "b", "c"))
  }

  test("set id") {
    var foo = instance[Foo]
    foo = foo.set(_.id) := 7L
    assert(foo.id === 7L)
  }

  test("time machine") {
    val beforeStart = clock()
    val foo = instance[Foo].set(_.name).to("version 1")
    val before = clock()
    val later = foo.set(_.name).to("version 2")
    val historic = later.timeMachine(before)
    historic.map {
      before =>
        assert(before === foo)
    }.getOrElse(fail("time machine did not find an object"))

    historic.get.timeMachine(time + 1).map {
      latest =>
        assert(latest === later)
    }.getOrElse(fail("could not move forward in time"))

    assert(later.timeMachine(beforeStart) === None)
    assert(instance[Foo].timeMachine(beforeStart) === None)

    val empty = instance[Foo]
    assert(empty.timeMachine(time) === Some(empty))

    // trying to modify an object from the past should fail
    evaluating {
      historic.get.set(_.name) := "not allowed!"
    } should produce[IllegalArgumentException]

    val a = instance[Foo].set(_.name) := "version 1"
    val b = a.set(_.name) := "version 2"
    val c = b.timeMachine(time + 1).get
    assert(b === c)

    // test some empty history/specific timestamp edge cases
    val beforeEdgeEmptyCreated = clock()
    val edgeEmpty = instance[Foo]
    val afterEdgeEmptyCreated = clock()
    val notEmpty = edgeEmpty.set(_.name) := "not empty"
    val edgeBackInTime = notEmpty.timeMachine(afterEdgeEmptyCreated).get
    assert(edgeBackInTime.timeMachine(beforeEdgeEmptyCreated) === None)
    assert(edgeBackInTime.timeMachine(beforeEdgeEmptyCreated + 1) === Some(edgeEmpty))
    assert(edgeBackInTime.timeMachine(beforeEdgeEmptyCreated + 2) === Some(edgeEmpty))
  }

  test("prune") {
    val empty = instance[Foo]
    val created = clock()
    val init = (empty set(_.name)  := "init")
    val inited = clock()
    val pruned = instance[Foo].set(_.name).to("init").prune

    assert(init.timeMachine(created) === Some(empty))
    assert(init.timeMachine(inited) === Some(init))
    assert(pruned.timeMachine(inited) === None)
  }

  test("forceCurrent") {
    val a = instance[Foo].set(_.name).to("a").prune
    val b = a.set(_.name) := "b"
    val c = b.set(_.name) := "c"
    val bp =  b.forceAfter(c)
    assert(bp === b)
    assert(bp.timeline.head === bp)
    assert(bp.timeline.drop(2).take(1).head === b)

    val cp = c :: bp
    assert(cp.timeline.take(3).toList === List(cp, c, bp))
  }

  test("forceCurrent & time machine") {
    val a = (instance[Foo].set(_.name) := "a").prune
    val b = a.set(_.name) := "b"
    val afterB = clock()
    val c = b.set(_.name) := "c"

    val bp = c.timeMachine(afterB).get
    evaluating(a.forceAfter(bp)) should produce[RuntimeException]
  }
}
