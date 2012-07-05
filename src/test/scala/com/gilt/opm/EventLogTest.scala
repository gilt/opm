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

case class Nested(id: Long, foo: Foo) extends HasId with Mutatable[Nested] {
  def this() = this(0L, null)
}

class EventLogTest extends FunSuite with ShouldMatchers {

  test("snapshot") {
    val obj = Nested(8, Foo(7, "hello, world"))
    val eventLog = EventLog.snapshot(obj)
    eventLog.reify should equal(obj)
  }

  test("snapshot text") {
    val obj = Nested(8, Foo(7, "hello, world"))
    val eventLog = EventLog.snapshot(obj)
    val textLog = eventLog.textLog
    println(textLog.mkString("\n"))
    val newEventLog = EventLog.reify[Nested](textLog)
    val obj2 = newEventLog.reify
    obj should equal(obj2)
  }
}
