package com.gilt.opm

import java.util.concurrent.TimeUnit
import org.scalatest.FunSuite
import com.giltgroupe.util.Timestamp
import com.giltgroupe.util.client.CommonsJson

case class TestClass(id: String, ts: NanoTimestamp)

/**
 * Testing suite for the NanoTimestamp class.
 *
 * @author: Ryan Martin
 * @since: 11/19/12 3:01 PM
 * @see NanoTimestamp
 */
class NanoTimestampTest extends FunSuite {
  test("compare same timestamps") {
    val ts1 = new NanoTimestamp()
    val ts2 = new NanoTimestamp(ts1.time)
    assert(ts1.equals(ts2))
    assert(ts1.hashCode == ts2.hashCode)
  }

  test("marshall into and out of string") {
    val ts1 = new NanoTimestamp()
    val ts2 = NanoTimestamp.valueOf(ts1.asString())
    assert(ts1.equals(ts2))
  }

  test("compare timestamps") {
    val ts1 = new NanoTimestamp()
    val ts2 = new NanoTimestamp(ts1.time + 1)
    val ts3 = new NanoTimestamp(ts1.time + 2)
    assert(ts1.compareTo(ts2) == -1)
    assert(ts3.compareTo(ts2) == 1)
    assert(ts1 < ts2)
    assert(ts1 <= ts2)
    assert(ts1 <= ts1)
    assert(ts2 > ts1)
    assert(ts2 >= ts1)
    assert(ts2 >= ts2)
  }

  test("add to timestamp") {
    val ts1 = new NanoTimestamp()
    val ts2 = new NanoTimestamp(ts1.time + 1)
    val ts3 = ts1 + 1
    assert(ts2 == ts3)
  }

  test("subtract from timestamp") {
    val ts1 = new NanoTimestamp()
    val ts2 = new NanoTimestamp(ts1.time - 1)
    val ts3 = ts1 - 1
    assert(ts2 == ts3)
  }
  test("create from Gilt timestamp") {
    val ts1 = new Timestamp()
    val ts2 = new NanoTimestamp(ts1)
    assert(ts1.getTime == TimeUnit.MILLISECONDS.convert(ts2.time, TimeUnit.NANOSECONDS))
  }

  test("create from Gilt timestamp with nanoseconds should ignore milliseconds in the Timestamp") {
    val ts1 = new Timestamp(1002) // 1.002s
    val ts2 = new NanoTimestamp(ts1, 10) // create with 10ns
    assert(ts2.time == 1000000010) // drops the 2ms and keeps the 10ns: 1.00000001s
  }

  test("timestamp toString") {
    assert((new NanoTimestamp(1)).toString == "Thu, 1 Jan 1970 00:00:00.000000001 UTC")
    assert((new NanoTimestamp(new Timestamp(1000), 1)).toString == "Thu, 1 Jan 1970 00:00:01.000000001 UTC")
  }

  test("string to timestamp") {
    assert(NanoTimestamp.valueOf("Thu, 1 Jan 1970 00:00:00.000000001 UTC").time == 1)
    assertTimestampsEqual(NanoTimestamp.valueOf("Thu, 1 Jan 1970 00:00:00.001000000 UTC"), new Timestamp(1))
    assertTimestampsEqual(NanoTimestamp.valueOf("Thu, 1 Jan 1970 00:00:00.001000 UTC"), new Timestamp(1))
    assertTimestampsEqual(NanoTimestamp.valueOf("Thu, 1 Jan 1970 00:00:00.001 UTC"), new Timestamp(1))
    assertTimestampsEqual(NanoTimestamp.valueOf("Thu, 1 Jan 1970 00:00:01 UTC"), new Timestamp(1000))
  }

  test("serialize into and out of JSON") {
    val test1 = TestClass("1234", new NanoTimestamp)
    val json = CommonsJson.generate(test1)
    val test2 = CommonsJson.parse[TestClass](json)
    assert(test1 == test2)
    assert(test1.ts.time == test2.ts.time)
  }

  def assertTimestampsEqual(nanoTimestamp: NanoTimestamp, timestamp: Timestamp) {
    assert(nanoTimestamp.time == TimeUnit.NANOSECONDS.convert(timestamp.getTime, TimeUnit.MILLISECONDS))
  }
}
