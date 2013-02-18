package com.gilt.opm

import org.codehaus.jackson.annotate.JsonCreator
import org.codehaus.jackson.annotate.JsonValue
import com.giltgroupe.util.Timestamp
import com.giltgroupe.util.RichLongForMeasurement._
import com.giltgroupe.util.MeasurementLong._
import com.giltgroupe.util.time.MonotonicClock

/**
 * Immutable wrapper around a timestamp.  Represents time the same as {@link com.giltgroupe.util.time.MonotonicClock},
 * but provides no real operations. This is necessary for usage in which {@link com.giltgroupe.util.Timestamp}'s
 * milliseconds is not accurate enough.
 *
 * @author: Ryan Martin
 * @since: 11/19/12 2:06 PM
 * @param time: Time like from {@link com.giltgroupe.util.time.MonotonicClock#currentTimeNanos()}.
 */
class NanoTimestamp(val time: Long) extends Comparable[NanoTimestamp] {

  /**
   * Constructs a new instance representing "now"
   */
  def this() = this(
    if (NanoTimestamp.getArtificialNow == 0) MonotonicClock.currentTimeNanos
    else NanoTimestamp.getArtificialNow
  )

  /**
   * Constructs a new instance from a Timestamp instance. Because of overlap with the nanoseconds and the unclear
   * definition of milliseconds in Timestamp (they get dropped in toString), this method ignores them in the Timestamp
   * and expects them to be passed in in the nanoseconds parameter.
   *
   * Intentionally not creating a method for the reverse conversion because of the loss of data (nanoseconds).
   *
   * @param timestamp {@link com.giltgroupe.util.Timestamp} instance
   * @param ns: The number of nanoseconds to append to the Timestamp (since it can't store them itself)
   */
  def this(timestamp: Timestamp, ns: Long) = this(
    ((timestamp.getTime milliseconds) in seconds in nanoseconds) + ns
  )

  /**
   * Constructs a new instance from a Timestamp instance. This is separate from above to make it explicitly clear
   * that nanoseconds is not passed in, and the milliseconds in the Timestamp should be preserved.
   *
   * @param timestamp {@link com.giltgroupe.util.Timestamp} instance
   */
  def this(timestamp: Timestamp) = this((timestamp.getTime milliseconds) in nanoseconds)

  /**
   * Compare this instance with equality against a different instance
   *
   * @param o Any instance, though only a Timestamp can possibly be equal
   * @return true if the times are the same, else false
   */
  override def equals(o: Any): Boolean = {
    o match {
      case nts: NanoTimestamp => time == nts.time
      case _ => false
    }
  }

  /**
   * @return a good hash code for this instance.
   */
  override def hashCode(): Int = time.##

  /**
   * Leaning on Timestamp.toString here, but it doesn't include fractional seconds, so I need to add that myself.
   *
   * @return a string representation
   */
  override def toString() = {
    val fractionalSeconds = time - ((time nanoseconds) in seconds in nanoseconds)
    """(\d\d:\d\d:\d\d) """.r replaceAllIn (new Timestamp((time nanoseconds) in milliseconds).toString, "$1.%09d ".format(fractionalSeconds))
  }

  override def compareTo(o: NanoTimestamp) = {
    // we don't just subtract here, since we have
    // a long, but this returns an int. Rather than
    // risk undefined behavior with big time spans,
    // better to just to be explicit
    (time - o.time) match {
      case lt if lt < 0 => -1
      case gt if gt > 0 => 1
      case _ => 0
    }
  }

  def >(o: NanoTimestamp) = compareTo(o) == 1

  def >=(o: NanoTimestamp) = compareTo(o) >= 0

  def <(o: NanoTimestamp) = compareTo(o) == -1

  def <=(o: NanoTimestamp) = compareTo(o) <= 0

  /**
   * Add nanoseconds to the current timestamp
   *
   * @param duration This should be in nanoseconds.
   */
  def +(duration: Long) = NanoTimestamp(this.time + duration)

  /**
   * Subtract nanoseconds to the current timestamp
   *
   * @param duration This should be in nanoseconds.
   */
  def -(duration: Long) = NanoTimestamp(this.time - duration)

  @JsonValue
  def asString(): String = toString()
}

object NanoTimestamp {
  private val artificialNow = new ThreadLocal[Long]()

  def apply(timestamp: Long) = new NanoTimestamp(timestamp)
  def apply(timestamp: Timestamp) = new NanoTimestamp(timestamp)
  def apply(timestamp: Timestamp, ns: Long) = new NanoTimestamp(timestamp, ns)

  def now = new NanoTimestamp()

  /**
   * FOR TESTING ONLY. Used to replace the clock with a fixed time.
   * @param now time value for "now" to use on this thread
   */
  def setArtificialNow(now: Long) {
    artificialNow.set(now)
  }
  def getArtificialNow() = artificialNow.get()

  /**
   * Need to parse out fractional seconds first, since Timestamp string parsing does not support it.
   */
  @JsonCreator
  def valueOf(timestamp: String): NanoTimestamp = {
    val timeMatcher = """(\d\d:\d\d:\d\d)(\.\d{1,9})? """.r // Match time and time decimal separately
    val matchedNano = Option(
        timeMatcher.findFirstMatchIn(timestamp).
          map {
          _.group(2)
          }.get
      ).getOrElse("").
        replace(".", "")
    new NanoTimestamp(
      Timestamp.valueOf(timeMatcher replaceFirstIn(timestamp, "$1 ")), // Remove the decimal portion
      "%-9s".format(matchedNano).replace(" ", "0").toLong  // Convert any decimal portion to nanoseconds. This pads 0s
                                                           // on the right and converts to Long, to avoid decimal rounding
                                                           // problems with Double or Float.
    )
  }
}
