package com.gilt.opm

import org.scalatest.FunSuite
import com.mongodb.casbah.MongoConnection
import java.util.{UUID, Date}

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/4/12 8:25 PM
 */
object MongoTest {
  trait TestDomain extends OpmObject {
    def id: Long
    def name: String
    def description: String
    def createdAt: Date
    def tags: Seq[String]
  }

  trait SimpleDomain extends OpmObject {
    def name: String
  }

  object SimpleDomain extends OpmMongoStorage[SimpleDomain] {
    val collection = MongoConnection()("opm-MongoTest")("opm_simple")
    collection.drop()
  }
}

class MongoTest extends FunSuite with OpmMongoStorage[MongoTest.TestDomain] {
  import MongoTest._
  import OpmFactory._
  val collection = MongoConnection()("opm-MongoTest")("opm")
  collection.drop()

  test("write 1000") {
    val count = 1000
    val key = 1.toString

    val obj = (1 to count).foldLeft(instance[TestDomain](key)) {
      case (i: TestDomain, n: Int) =>
        i.set(_.id).to(n)
    }
    assert(obj.timeline.size === count + 1)
    put(obj)

    val loaded = get(key)
    assert(loaded.get.id === count.toLong)
    val history = loaded.get.timeline
    assert(history.init.size === count)
    history.init.zipWithIndex.foreach(t => assert(t._1.id + t._2 === count, "%s -> %d".format(t, t._1.id + t._2)))

    remove(key)
    val obj2 = history.drop(count/2).head
    put(obj2)
    val reloaded = get(key)
    assert(reloaded.get.timeline.size === obj2.timeline.size)
    assert(reloaded.get.id === count/2.toLong)
    val historyReloaded = reloaded.get.timeline
    historyReloaded.zip(obj2.timeline).foreach(x => assert(x._1 === x._2))
  }

  test("phase != 0 write ordering works") {
    val key = 2.toString
    val obj = instance[TestDomain](key).set(_.id).to(1).set(_.id).to(2).set(_.id).to(3).set(_.id).to(4)
    assert(obj.timeline.size === 5)

    put(obj)
    val obj2 = obj.set(_.id).to(5).set(_.id).to(6)
    put(obj2)
    val obj3 = get(key).get
    for (i <- 6 to 1 by -1) {
      assert(obj3.timeline.drop(6 - i).head.id === i)
    }
  }

  test("overwrite succeeds") {
    import MongoTest.SimpleDomain
    val d1 =
      OpmFactory.instance[SimpleDomain]("sd1").
        set(_.name).to("d1").
        prune

    val d2 =
      OpmFactory.instance[SimpleDomain]("sd1").
        set(_.name).to("d2").
        prune

    SimpleDomain.put(d1)
    SimpleDomain.put(d2)

    val d3 = SimpleDomain.get("sd1")
    assert(d3.map(_.name) === Some("d2"))
  }

  test("default to/from MongoMapper works for lists") {
    val d1 =
      OpmFactory.instance[TestDomain]("td1").
        set(_.name).to("name1").
        set(_.tags).to(Seq("tag1"))
    put(d1)

    val d2 = get("td1").get
    assert(d1.tags == Seq("tag1"))
    assert(d2.tags == d1.tags)

    val d3 =
      OpmFactory.instance[TestDomain]("td1").
        set(_.name).to("name1").
        set(_.tags).to(Seq("tag1", "tag2"))
    put(d3)

    val d4 = get("td1").get
    assert(d4.tags != d1.tags)
    assert(d4.tags == Seq("tag1", "tag2"))
    assert(d4.tags == d3.tags)
  }

  test("getUpdatedKeys finds all when no range is given") {
    val objects = setupForGetUpdatedKeys()
    val result = getUpdatedKeys(None, None)
    objects.foreach {
      obj => assert(result.exists(_ == obj.opmKey))
    }
  }

  test("getUpdatedKeys finds within range") {
    val objects = setupForGetUpdatedKeys()
    val result = getUpdatedKeys(Option(NanoTimestamp(objects.head.opmTimestamp + 1)), Option(NanoTimestamp(objects.tail.head.opmTimestamp + 1)))
    assert(!result.exists(_ == objects.head.opmKey))
    assert(result.exists(_ == objects.tail.head.opmKey))
    assert(!result.exists(_ == objects.last.opmKey))
  }

  test("getUpdatedKeys finds with lower bound") {
    val objects = setupForGetUpdatedKeys()
    val result = getUpdatedKeys(Option(NanoTimestamp(objects.head.opmTimestamp + 1)), None)
    assert(!result.exists(_ == objects.head.opmKey))
    assert(result.exists(_ == objects.tail.head.opmKey))
    assert(result.exists(_ == objects.last.opmKey))
  }

  test("getUpdatedKeys finds with upper bound") {
    val objects = setupForGetUpdatedKeys()
    val result = getUpdatedKeys(None, Option(NanoTimestamp(objects.tail.head.opmTimestamp + 1)))
    assert(result.exists(_ == objects.head.opmKey))
    assert(result.exists(_ == objects.tail.head.opmKey))
    assert(!result.exists(_ == objects.last.opmKey))
  }

  test("getUpdatedKeys finds updated keys within range") {
    val objects = setupForGetUpdatedKeys()
    val d1 =
      OpmFactory.instance[TestDomain](objects.head.opmKey).
        set(_.name).to("updated_name")
    squashPut(d1)
    val result = getUpdatedKeys(Option(NanoTimestamp(objects.last.opmTimestamp + 1)), None)
    assert(result.exists(_ == objects.head.opmKey))
    objects.tail.foreach {
      obj => assert(!result.exists(_ == obj.opmKey))
    }
  }

  test("search by equals for single property") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("search_single_name1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_single_name2")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_single_name2")
    squashPut(d3)

    val results1 = search(_.name).equals("search_single_name1")
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d1))

    val results2 = search(_.name).equals("search_single_name2")
    assert(results2.all.length == 2)
    assert(results2.all.exists(opmObjectMatches(_, d2)))
    assert(results2.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by equals for multiple properties") {
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_equals_name1").
        set(_.description).to("search_equals_desc1")
    squashPut(d1)
    val key2 = uniqueKey
    val d2 =
      OpmFactory.instance[TestDomain](key2).
        set(_.name).to("search_equals_name2").
        set(_.description).to("search_equals_desc1")
    squashPut(d2)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key2).get.set(_.createdAt).to(new Date)))
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_equals_name3").
        set(_.description).to("search_equals_desc3")
    squashPut(d3)

    val results1 = search(_.description).equals("search_equals_desc1").search(_.name).equals("search_equals_name2")
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d2))

    val results2 = search(_.name).equals("search_equals_name2").search(_.description).equals("search_equals_desc3")
    assert(results2.all.length == 0)
  }

  test("search by query ignores old updates") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("search_updated_name1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))

    val key2 = uniqueKey
    val d2 =
      OpmFactory.instance[TestDomain](key2).
        set(_.name).to("search_updated_name1")
    squashPut(d2)

    val results1 = search(_.name).equals("search_updated_name1")
    assert(results1.all.length == 2)
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))

    0 until 10 foreach (i => squashPut(get(key2).get.set(_.name).to("search_updated_name" + i)))

    val results2 = search(_.name).equals("search_updated_name1")
    assert(results2.all.length == 1)
    assert(opmObjectMatches(results2.all.head, d1))

    val d3 = get(key2).get
    assert(d3.name != "search_updated_name1")
    assert(d3.timeline.exists(hist => hist.name == "search_updated_name1"))
  }

  test("search by blank for first query") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("name1").
        set(_.description).to("description1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("name2")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("name2")
    squashPut(d3)

    val results = search(_.description).isBlank()
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by blank for subsequent query") {
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_blank").
        set(_.description).to("desc")
    squashPut(d1)
    val d2 =
      OpmFactory.instance[TestDomain]("uniqueKey").
        set(_.name).to("search_blank")
    squashPut(d2)

    val results1 = search(_.name).equals("search_blank").search(_.description).isBlank()
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d2))
  }

  test("search by between for first query") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("1").
        set(_.description).to("desc")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("2").
        set(_.description).to("desc")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("3").
        set(_.description).to("desc")
    squashPut(d3)
    val d4 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("4").
        set(_.description).to("desc")
    squashPut(d4)

    val results = search(_.name).between("2", "3").search(_.description).equals("desc")
    assert(!results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.all.exists(opmObjectMatches(_, d3)))
    assert(!results.all.exists(opmObjectMatches(_, d4)))
  }

  test("search by between for subsequent query") {
    val key = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key).
        set(_.name).to("search_bet").
        set(_.description).to("1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_bet").
        set(_.description).to("2")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_bet").
        set(_.description).to("3")
    squashPut(d3)
    val d4 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_bet").
        set(_.description).to("4")
    squashPut(d4)
    val results = search(_.name).equals("search_bet").search(_.description).between("2", "3")
    assert(!results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.all.exists(opmObjectMatches(_, d3)))
    assert(!results.all.exists(opmObjectMatches(_, d4)))
  }

  test("search by between exclusive for first query") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("1").
        set(_.description).to("desc")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("2").
        set(_.description).to("desc")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("3").
        set(_.description).to("desc")
    squashPut(d3)
    val d4 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("4").
        set(_.description).to("desc")
    squashPut(d4)

    val results = search(_.name).betweenExclusive("1", "4").search(_.description).equals("desc")
    assert(!results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.all.exists(opmObjectMatches(_, d3)))
    assert(!results.all.exists(opmObjectMatches(_, d4)))
  }

  test("search by between exclusive for subsequent query") {
    val key = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key).
        set(_.name).to("search_bet_excl").
        set(_.description).to("1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_bet_excl").
        set(_.description).to("2")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_bet_excl").
        set(_.description).to("3")
    squashPut(d3)
    val d4 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_bet_excl").
        set(_.description).to("4")
    squashPut(d4)
    val results = search(_.name).equals("search_bet_excl").search(_.description).betweenExclusive("1", "4")
    assert(!results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.all.exists(opmObjectMatches(_, d3)))
    assert(!results.all.exists(opmObjectMatches(_, d4)))
  }

  test("search by greater than for first query") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("1").
        set(_.description).to("desc")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("2").
        set(_.description).to("desc")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("3").
        set(_.description).to("desc")
    squashPut(d3)

    val results = search(_.name).greaterThan("1").search(_.description).equals("desc")
    assert(!results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by greater than for subsequent query") {
    val key = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key).
        set(_.name).to("search_gt").
        set(_.description).to("1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_gt").
        set(_.description).to("2")
    squashPut(d2)
    val results1 = search(_.name).equals("search_gt").search(_.description).greaterThan("1")
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d2))
  }

  test("search by greater than or equal for first query") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("1").
        set(_.description).to("desc")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("2").
        set(_.description).to("desc")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("3").
        set(_.description).to("desc")
    squashPut(d3)

    val results = search(_.name).greaterThanOrEqual("2").search(_.description).equals("desc")
    assert(!results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by greater than or equal for subsequent query") {
    val key = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key).
        set(_.name).to("search_gte").
        set(_.description).to("1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_gte").
        set(_.description).to("2")
    squashPut(d2)
    val results1 = search(_.name).equals("search_gte").search(_.description).greaterThanOrEqual("2")
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d2))
  }

  test("search by less than for first query") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("1").
        set(_.description).to("desc")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("2").
        set(_.description).to("desc")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("3").
        set(_.description).to("desc")
    squashPut(d3)

    val results = search(_.name).lessThan("3").search(_.description).equals("desc")
    assert(results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(!results.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by less than for subsequent query") {
    val key = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key).
        set(_.name).to("search_lt").
        set(_.description).to("1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_lt").
        set(_.description).to("2")
    squashPut(d2)
    val results1 = search(_.name).equals("search_lt").search(_.description).lessThan("2")
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d1))
  }

  test("search by less than or equal for first query") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("1").
        set(_.description).to("desc")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("2").
        set(_.description).to("desc")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("3").
        set(_.description).to("desc")
    squashPut(d3)

    val results = search(_.name).lessThanOrEqual("2").search(_.description).equals("desc")
    assert(results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(!results.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by less than or equal for subsequent query") {
    val key = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key).
        set(_.name).to("search_lte").
        set(_.description).to("1")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_lte").
        set(_.description).to("2")
    squashPut(d2)
    val results1 = search(_.name).equals("search_lte").search(_.description).lessThanOrEqual("1")
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d1))
  }

  test("search with limit succeeds") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("search_limit1").
        set(_.description).to("search_limit")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain]("uniqueKey").
        set(_.name).to("search_limit2").
        set(_.description).to("search_limit")
    squashPut(d2)

    val results = search(_.description).equals("search_limit")
    assert(results.all.length == 2)
    assert(results.all.exists(opmObjectMatches(_, d1)))
    assert(results.all.exists(opmObjectMatches(_, d2)))
    assert(results.limit(1).all.length == 1)
    assert(results.all.exists(opmObjectMatches(_, d1)) || results.all.exists(opmObjectMatches(_, d2)))
  }

  // Testing this here because it requires an implementation of get and put
  test("squashPut succeeds") {
    val key = uniqueKey
    val obj1 = instance[TestDomain](key).
      set(_.id).to(1).
      set(_.name).to("name1")
    put(obj1)

    // Straight put, without a prune
    val obj2 = toSetter(get(key).get)
    assert(obj2.timeline.size == 3)
    // One step back, both are set
    assert(obj2.timeline.head.id == 1)
    assert(obj2.timeline.head.name == "name1")
    // Two steps back, only id is set
    assert(obj2.timeline.tail.head.id == 1)
    assertOpmObjectAccessorFails(() => obj2.timeline.tail.head.name)
    // Three steps back, nothing is set
    assertOpmObjectAccessorFails(() => obj2.timeline.tail.tail.head.id)
    assertOpmObjectAccessorFails(() => obj2.timeline.tail.tail.head.name)

    val obj3 = instance[TestDomain](key).
      set(_.id).to(2).
      set(_.name).to("name2")
    squashPut(obj3)

    // With a squashed put
    val obj4 = toSetter(get(key).get)
    assert(obj4.timeline.size == 4)
    // One step back, both are set in the same diff
    assert(obj4.timeline.head.id == 2)
    // Two steps back, it meets the previous timeline
    assert(obj4.timeline.head.name == "name2")
    assert(obj4.timeline.tail == obj2.timeline)
  }

  def assertOpmObjectAccessorFails(f: () => Any) {
    try {
      assert(f() == null)
      assert(false)
    } catch {
      case e: NoSuchElementException => assert(true)
      case _ => assert(false)
    }

  }

  def opmObjectMatches(obj1: OpmObject, obj2: OpmObject): Boolean = {
    obj1.opmKey == obj2.opmKey
  }

  def setupForGetUpdatedKeys() = {
    1 to 3 map {
      i =>
        val key = uniqueKey
        put(OpmFactory.instance[TestDomain](key).
          set(_.name).to("name" + i)
        )
        1 to 10 foreach (i => squashPut(get(key).get.set(_.createdAt).to(new Date)))
        get(key).get
    }
  }

  def uniqueKey = UUID.randomUUID().toString
}
