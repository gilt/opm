package com.gilt.opm.utils

/**
 * Defines a clock which has essentially millisecond accuracy, but which returns times in nanoseconds so that
 * it can include a monotonically-increasing counter so that this clock will never return the same time twice.
 *
 * Note that if you take currentTimeNanos / 1000000, you'll get the value returned from currentTimeMillis(),
 * guaranteed.
 *
 * The purpose of this clock is not really to measure time. Instead, you should think of it as something like
 * a database sequence that includes some help data about when the value was generated.  It's trying to work
 * around the situation where we'd really like to use System.currentTimeMillis(), but we need every call to
 * return a unique value.
 *
 * If the system clock moves backwards, the instance will hold on to the highest value it has seen.  Each time
 * it is called, it increments a counter, and has enough room for essentially 1M calls against it without the
 * clock moving forward, before it can no longer guarantee that it is returning a unique value. So this is likely
 * to be fine unless the clock moves so far back in time, and the frequency of calls against the clock is so
 * high, that it cannot catch up. In that case the clock will throw an exception.
 *
 * Note that this clock will stop working at Sat Apr 12 00:47:16 IST 2262. If Gilt is still a viable venture
 * 250 years from now, and still depending on this code, accept my humble apologies. :)
 */
trait MonotonicClock {
  private [this] var counter = 0
  private [this] var lastClock = 0l
  private [this] val NanosPerMs = 1000000

  protected def millisecondClock(): Long = System.currentTimeMillis()

  def currentTimeNanos(): Long = {
    val now = NanosPerMs * millisecondClock()
    synchronized {
      if (now <= lastClock) {     // handle possibility of clock moving backwards for leap seconds by checking for <
        counter += 1
        assert(counter < NanosPerMs,
          "Counter has reached %s without the clock moving forward: %s".format(counter, lastClock))
      } else {
        lastClock = now
        counter = 0
      }
      lastClock + counter
    }
  }
}

object MonotonicClock extends MonotonicClock
