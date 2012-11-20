package com.giltgroupe.util

import org.scalatest.FunSuite
import RichLongForMeasurement._
import java.util.concurrent.TimeUnit._

/**
 * Document me.
 *
 * @author: Ryan Martin
 * @since: 11/26/12 2:18 PM
 */
class MeasurementLongTest extends FunSuite {
  test("various TimeUnit conversions using the DSL") {
    assert(((1 days) in milliseconds) == MeasurementLong(86400000, MILLISECONDS))
    assert(((86400000 milliseconds) in days) == MeasurementLong(1, DAYS))
    assert(((1 milliseconds) in nanoseconds) == MeasurementLong(1000000, NANOSECONDS))
    assert(((4000000 nanoseconds) in milliseconds) == MeasurementLong(4, MILLISECONDS))
    assert(((1 milliseconds) in microseconds) == MeasurementLong(1000, MICROSECONDS))
    assert(((4000 microseconds) in milliseconds) == MeasurementLong(4, MILLISECONDS))
    assert(((1 seconds) in milliseconds) == MeasurementLong(1000, MILLISECONDS))
    assert(((4000 milliseconds) in seconds) == MeasurementLong(4, SECONDS))
  }

  test("identity conversion") {
    assert(((1 nanoseconds) in nanoseconds) == MeasurementLong(1, NANOSECONDS))
  }

  test("round-trip conversion") {
    assert(((1 seconds) in milliseconds in seconds) == MeasurementLong(1, SECONDS))
  }

  test("truncates for lower granularity") {
    assert(((1 nanoseconds) in milliseconds) == MeasurementLong(0, MILLISECONDS))
    assert(((499000 nanoseconds) in milliseconds) == MeasurementLong(0, MILLISECONDS))
    assert(((500000 nanoseconds) in milliseconds) == MeasurementLong(0, MILLISECONDS))
    assert(((999999 nanoseconds) in milliseconds) == MeasurementLong(0, MILLISECONDS))
    assert(((1000000 nanoseconds) in milliseconds) == MeasurementLong(1, MILLISECONDS))
  }
}
