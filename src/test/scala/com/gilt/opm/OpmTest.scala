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

  test("prune") {
    val empty = instance[Foo]
    val init = (empty set(_.name)  := "init")
    val pruned = init.set(_.name).to("ready").prune
    assert(pruned.timeline.size === 1)

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

  test("timestamp") {
    val before = clock()
    val a = instance[Foo].set(_.name) := "a"
    assert(a.timestamp === before + 2)
  }

  test("branching") {
    val a = instance[Foo].set(_.name).to("a").prune
    val b = a.set(_.name) := "b"
    val c = b.set(_.name) := "c"
    val d = c.set(_.name) := "d"
    val e = d.set(_.name) := "e"

    val f = c.set(_.name) := "f"
    val g = f.set(_.name) := "g"

    assert(e.timeline.toList === List(e,d,c,b,a))
    assert(g.timeline.toList === List(g, f, c, b, a))
  }
}
