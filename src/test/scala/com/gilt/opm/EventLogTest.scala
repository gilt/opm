package com.gilt.opm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 7/5/12 11:14 AM
 */

case class Foo(id: Long, name: String) extends HasId with Mutatable[Foo] {
  def this() = this(0L, "")
}

class EventLogTest extends FunSuite with ShouldMatchers {

  test("snapshot") {
    val obj = Foo(7, "hello, world")
    val eventLog = EventLog.snapshot(obj)
    eventLog.events.map(_.command).toList should equal(List(
      Command(CreateOp, None, classOf[Foo]),
      Command(SetOp, Some("id"), 7L),
      Command(SetOp, Some("name"), "hello, world")))

    eventLog.reify should equal(obj)
  }
}
