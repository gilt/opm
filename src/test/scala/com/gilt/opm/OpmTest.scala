package com.gilt.opm

import org.scalatest.FunSuite

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 7/9/12 1:58 PM
 */
trait Foo {
  def id: Long

  def name: String

  def bar: Bar
}

trait Bar {
  def name: String
  def things: Set[String]
}


class OpmTest extends FunSuite {

  test("basics") {

    import InstanceFactory._

    val foo = InstanceFactory.instance[Foo]

    val foo2 = foo.set(_.name) := "test"

    assert(foo2.name === "test")

    val foo3 = foo2.set(_.bar).to(InstanceFactory.instance[Bar])
    println(foo3)
    val foo4 = foo3.set(_.bar.name).to("a bar")

    println(foo4)

    assert(foo4.bar.name === "a bar")

    val foo5 = foo4.set(_.bar.things) to(Set("a,b,c".split(","):_*))
    assert(foo5.bar.things === Set("a", "b", "c"))
  }
}
