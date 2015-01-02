package com.gilt.opm.utils

import java.util.concurrent.CopyOnWriteArraySet
import java.util.concurrent.atomic.{AtomicLong, AtomicInteger}
import scala.collection.JavaConverters._
import org.scalatest.{FunSuite, Matchers}


/**
 * Confirms that a bunch of threads hitting the MonotonicClock concurrently never see the same value.
 */
class MonoticClockTest extends FunSuite with Matchers {

  test("Clock never returns the same value twice") {
    val seen = new CopyOnWriteArraySet[Long]().asScala
    (1 to 10000).par.foreach { _ =>
      val now = MonotonicClock.currentTimeNanos()
      seen should not contain(now)
      seen.add(now)
    }
  }

  test("still increases even with leap seconds") {
    val clock = new MonotonicClock {
      val incr = new AtomicInteger(0)
      val clock = new AtomicLong(0l)
      override def millisecondClock(): Long = {
        if ((incr.incrementAndGet() % 5) == 0) {
          clock.get() - 1
        } else {
          clock.incrementAndGet()
        }
      }
    }
    var prev = clock.currentTimeNanos()
    (1 to 100000) foreach { _ =>
      val now = clock.currentTimeNanos()
      assert(now > prev)
      prev = now
    }
  }

  test("Clock blows exception if it cannot move forward") {
    val clock = new MonotonicClock {
      override def millisecondClock(): Long = 0
    }
    (1 to 999999) foreach { _ => clock.currentTimeNanos() }
    an [AssertionError] should be thrownBy clock.currentTimeNanos()
  }
}
