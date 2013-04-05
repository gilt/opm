package com.gilt.opm

import org.scalatest.FunSuite
import java.util.{UUID, Date}
import java.util.concurrent.TimeUnit
import scala.NoSuchElementException
import org.scalatest.matchers.ShouldMatchers
import com.giltgroupe.util.{GUID, CompactGuid}

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
    def count: Long
    def maybeName: Option[String]
    def maybeLong: Option[Long]
    def byte: Byte
    def char: Char
    def short: Short
    def int: Int
    def float: Float
    def double: Double
    def maybeFloat: Option[Float]
    def description: String
    def createdAt: Date
    def tags: Seq[String]
    def tagSet: Set[String]
    def guid: CompactGuid[TestDomain]
    def other_guid: Option[CompactGuid[TestDomain]]
  }

  trait SimpleDomain extends OpmObject {
    def name: String
  }

  object SimpleDomain extends OpmMongoStorage[SimpleDomain] with CollectionHelper {
    override val collectionName = "opm_simple"
  }

  trait Named extends OpmObject {
    def name: String
  }

  trait TestClass extends Named {
    def id: Long
  }

  object TestClass extends OpmMongoStorage[TestClass] with CollectionHelper {
    override val collectionName = "opm_test_class"
  }

  trait NamedAudited extends OpmAuditedObject[GUID] {
    def name: String
  }

  trait TestClassAudited extends NamedAudited {
    def id: Long
  }

  object TestClassAudited extends OpmAuditedMongoStorage[TestClassAudited, GUID] with CollectionHelper {
    override val collectionName = "opm_test_class"

    override def toMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] = {
      Some {
        {
          case (OpmIntrospection.UpdatedByField, _, Some(guid)) => guid.toString
        }
      }
    }

    override def fromMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] = {
      Some {
        {
          case (OpmIntrospection.UpdatedByField, _, str) => new GUID(str.toString)
        }
      }
    }
  }

}

class MongoTest extends FunSuite with OpmMongoStorage[MongoTest.TestDomain] with CollectionHelper with ShouldMatchers {
  import MongoTest._
  import OpmFactory._
  override val collectionName = "opm"

  override def toMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] = {
    Some {
      {
        case ("guid", _, guid) => guid.toString
        case ("other_guid", _, guid@Some(g)) => "xx" + g.toString + "xx"
      }
    }
  }

  override def fromMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] = {
    Some {
      {
        case ("guid", _, str) => CompactGuid.apply[TestDomain](str.toString)
        case ("other_guid", _, str) => CompactGuid.apply[TestDomain](str.toString.drop(2).dropRight(2))
      }
    }
  }

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

  test("empty collections") {
    import com.gilt.opm.OpmFactory._
    val obj = instance[TestDomain]("x")
      .set(_.tagSet).to(Set.empty[String])
    put(obj)

    val issue = get("x").get
    assert(issue.tagSet == Set.empty[String])
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

  test("allRecords") {
    val key1 = uniqueKey
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.name).to("all_records")
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("all_records")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("all_records")
    squashPut(d3)

    val results1 = allRecords
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = allRecords.limit(2)
    assert(results2.all.length == 2)
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

  test("search by equals for single non-standard property") {
    val key1 = uniqueKey
    val guid1 = CompactGuid.randomCompactGuid[TestDomain]()
    val guid2 = CompactGuid.randomCompactGuid[TestDomain]()
    val d1 =
      OpmFactory.instance[TestDomain](key1).
        set(_.guid).to(guid1)
    squashPut(d1)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key1).get.set(_.createdAt).to(new Date)))
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.guid).to(guid2)
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.guid).to(guid2)
    squashPut(d3)

    val results1 = search(_.guid).equals(guid1)
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d1))

    val results2 = search(_.guid).equals(guid2)
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

  test("search by equals for multiple non-standard properties") {
    val guid1 = CompactGuid.randomCompactGuid[TestDomain]()
    val guid2 = CompactGuid.randomCompactGuid[TestDomain]()
    val other_guid1 = Option(CompactGuid.randomCompactGuid[TestDomain]())
    val other_guid2 = Option(CompactGuid.randomCompactGuid[TestDomain]())
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.guid).to(guid1).
        set(_.other_guid).to(other_guid1)
    squashPut(d1)
    val key2 = uniqueKey
    val d2 =
      OpmFactory.instance[TestDomain](key2).
        set(_.guid).to(guid2).
        set(_.other_guid).to(other_guid1)
    squashPut(d2)
    // Push searched-for property down in the stack
    0 until 10 foreach (i => squashPut(get(key2).get.set(_.createdAt).to(new Date)))
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.guid).to(CompactGuid.randomCompactGuid[TestDomain]()).
        set(_.other_guid).to(other_guid2)
    squashPut(d3)

    val results1 = search(_.guid).equals(guid2).search(_.other_guid).equals(other_guid1)
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d2))

    val results2 = search(_.other_guid).equals(other_guid1).search(_.guid).equals(guid2)
    assert(results2.all.length == 1)
    assert(opmObjectMatches(results2.all.head, d2))

    val results3 = search(_.guid).equals(guid2).search(_.other_guid).equals(other_guid2)
    assert(results3.all.length == 0)

    val results4 = search(_.other_guid).equals(other_guid2).search(_.guid).equals(guid2)
    assert(results4.all.length == 0)
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

  test("search by 'not' for first query") {
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
        set(_.name).to("name2").
        set(_.description).to("description2")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("name2")
    squashPut(d3)

    val results1 = search(_.description).not().isBlank()
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(!results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.description).not().equals("description2")
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(results2.all.exists(opmObjectMatches(_, d3)))

    val results3 = search(_.description).not().lt("description2")
    assert(!results3.all.exists(opmObjectMatches(_, d1)))
    assert(results3.all.exists(opmObjectMatches(_, d2)))
    assert(!results3.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by 'not' for subsequent query") {
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_not").
        set(_.description).to("desc")
    squashPut(d1)
    val d2 =
      OpmFactory.instance[TestDomain]("uniqueKey").
        set(_.name).to("search_not")
    squashPut(d2)

    val results1 = search(_.name).equals("search_not").search(_.description).not().isBlank()
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d1))

    val results2 = search(_.name).equals("search_not").search(_.description).not().equals("desc")
    assert(results2.all.length == 1)
    assert(opmObjectMatches(results2.all.head, d2))

    val results3 = search(_.name).equals("search_not").search(_.description).not().lte("desc")
    assert(results3.all.length == 1)
    assert(opmObjectMatches(results3.all.head, d2))
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

  test("search by contains for first query") {
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_contains1").
        set(_.tags).to(Seq("tag1", "tag2"))
    squashPut(d1)
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_contains1").
        set(_.tags).to(Seq("tag4", "tag2"))
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_contains1").
        set(_.tags).to(Seq("tag3", "tag1"))
    squashPut(d3)

    val results1 = search(_.tags).contains("tag1").search(_.name).equals("search_contains1")
    assert(results1.all.length == 2)
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(!results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.tags).not().contains("tag1").search(_.name).equals("search_contains1")
    assert(results2.all.length == 1)
    assert(!results2.all.exists(opmObjectMatches(_, d1)))
    assert(results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))

    val results3 = search(_.tags).contains("tag3").search(_.name).equals("search_contains1")
    assert(results3.all.length == 1)
    assert(!results3.all.exists(opmObjectMatches(_, d1)))
    assert(!results3.all.exists(opmObjectMatches(_, d2)))
    assert(results3.all.exists(opmObjectMatches(_, d3)))

    val results4 = search(_.tags).not().contains("tag3").search(_.name).equals("search_contains1")
    assert(results4.all.length == 2)
    assert(results4.all.exists(opmObjectMatches(_, d1)))
    assert(results4.all.exists(opmObjectMatches(_, d2)))
    assert(!results4.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by contains for subsequent query") {
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_contains2").
        set(_.tags).to(Seq("tag1", "tag2"))
    squashPut(d1)
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_contains2").
        set(_.tags).to(Seq("tag4", "tag2"))
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_contains2").
        set(_.tags).to(Seq("tag3", "tag1"))
    squashPut(d3)

    val results1 = search(_.name).equals("search_contains2").search(_.tags).contains("tag1")
    assert(results1.all.length == 2)
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(!results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.name).equals("search_contains2").search(_.tags).not().contains("tag1")
    assert(results2.all.length == 1)
    assert(!results2.all.exists(opmObjectMatches(_, d1)))
    assert(results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))

    val results3 = search(_.name).equals("search_contains2").search(_.tags).contains("tag3")
    assert(results3.all.length == 1)
    assert(!results3.all.exists(opmObjectMatches(_, d1)))
    assert(!results3.all.exists(opmObjectMatches(_, d2)))
    assert(results3.all.exists(opmObjectMatches(_, d3)))

    val results4 = search(_.name).equals("search_contains2").search(_.tags).not().contains("tag3")
    assert(results4.all.length == 2)
    assert(results4.all.exists(opmObjectMatches(_, d1)))
    assert(results4.all.exists(opmObjectMatches(_, d2)))
    assert(!results4.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by 'in' for first query") {
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_in1").
        set(_.description).to("desc1")
    squashPut(d1)
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_in1")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_in1").
        set(_.description).to("desc3")
    squashPut(d3)

    val results1 = search(_.description).in(Set("desc1", "desc2")).search(_.name).equals("search_in1")
    assert(results1.all.length == 1)
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(!results1.all.exists(opmObjectMatches(_, d2)))
    assert(!results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.description).not().in(Set("desc1", "desc2")).search(_.name).equals("search_in1")
    assert(results2.all.length == 2)
    assert(!results2.all.exists(opmObjectMatches(_, d1)))
    assert(results2.all.exists(opmObjectMatches(_, d2)))
    assert(results2.all.exists(opmObjectMatches(_, d3)))

    val results3 = search(_.description).in(Set("desc1", "desc3")).search(_.name).equals("search_in1")
    assert(results3.all.length == 2)
    assert(results3.all.exists(opmObjectMatches(_, d1)))
    assert(!results3.all.exists(opmObjectMatches(_, d2)))
    assert(results3.all.exists(opmObjectMatches(_, d3)))

    val results4 = search(_.description).not().in(Set("desc1", "desc3")).search(_.name).equals("search_in1")
    assert(results4.all.length == 1)
    assert(!results4.all.exists(opmObjectMatches(_, d1)))
    assert(results4.all.exists(opmObjectMatches(_, d2)))
    assert(!results4.all.exists(opmObjectMatches(_, d3)))
  }

  test("search by 'in' for subsequent query") {
    val d1 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_in2").
        set(_.description).to("desc1")
    squashPut(d1)
    val d2 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_in2")
    squashPut(d2)
    val d3 =
      OpmFactory.instance[TestDomain](uniqueKey).
        set(_.name).to("search_in2").
        set(_.description).to("desc3")
    squashPut(d3)

    val results1 = search(_.name).equals("search_in2").search(_.description).in(Set("desc1", "desc2"))
    assert(results1.all.length == 1)
    assert(opmObjectMatches(results1.all.head, d1))

    val results2 = search(_.name).equals("search_in2").search(_.description).in(Set("desc1", "desc3"))
    assert(results2.all.length == 2)
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(results2.all.exists(opmObjectMatches(_, d3)))
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

    val results1 = search(_.name).between("2", "3").search(_.description).equals("desc")
    assert(!results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))
    assert(!results1.all.exists(opmObjectMatches(_, d4)))

    val results2 = search(_.name).not().between("2", "3").search(_.description).equals("desc")
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))
    assert(results2.all.exists(opmObjectMatches(_, d4)))
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

    val results1 = search(_.name).equals("search_bet").search(_.description).between("2", "3")
    assert(!results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))
    assert(!results1.all.exists(opmObjectMatches(_, d4)))

    val results2 = search(_.name).equals("search_bet").search(_.description).not().between("2", "3")
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))
    assert(results2.all.exists(opmObjectMatches(_, d4)))
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

    val results1 = search(_.name).betweenExclusive("1", "4").search(_.description).equals("desc")
    assert(!results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))
    assert(!results1.all.exists(opmObjectMatches(_, d4)))

    val results2 = search(_.name).not().betweenExclusive("1", "4").search(_.description).equals("desc")
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))
    assert(results2.all.exists(opmObjectMatches(_, d4)))
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

    val results1 = search(_.name).equals("search_bet_excl").search(_.description).betweenExclusive("1", "4")
    assert(!results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))
    assert(!results1.all.exists(opmObjectMatches(_, d4)))

    val results2 = search(_.name).equals("search_bet_excl").search(_.description).not().betweenExclusive("1", "4")
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))
    assert(results2.all.exists(opmObjectMatches(_, d4)))
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

    val results1 = search(_.name).greaterThan("1").search(_.description).equals("desc")
    assert(!results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.name).not().greaterThan("1").search(_.description).equals("desc")
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))
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

    val results2 = search(_.name).equals("search_gt").search(_.description).not().greaterThan("1")
    assert(results2.all.length == 1)
    assert(opmObjectMatches(results2.all.head, d1))
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

    val results1 = search(_.name).greaterThanOrEqual("2").search(_.description).equals("desc")
    assert(!results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.name).not().greaterThanOrEqual("2").search(_.description).equals("desc")
    assert(results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(!results2.all.exists(opmObjectMatches(_, d3)))
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

    val results2 = search(_.name).equals("search_gte").search(_.description).not().greaterThanOrEqual("2")
    assert(results2.all.length == 1)
    assert(opmObjectMatches(results2.all.head, d1))
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

    val results1 = search(_.name).lessThan("3").search(_.description).equals("desc")
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(!results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.name).not().lessThan("3").search(_.description).equals("desc")
    assert(!results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(results2.all.exists(opmObjectMatches(_, d3)))
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

    val results2 = search(_.name).equals("search_lt").search(_.description).not().lessThan("2")
    assert(results2.all.length == 1)
    assert(opmObjectMatches(results2.all.head, d2))
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

    val results1 = search(_.name).lessThanOrEqual("2").search(_.description).equals("desc")
    assert(results1.all.exists(opmObjectMatches(_, d1)))
    assert(results1.all.exists(opmObjectMatches(_, d2)))
    assert(!results1.all.exists(opmObjectMatches(_, d3)))

    val results2 = search(_.name).not().lessThanOrEqual("2").search(_.description).equals("desc")
    assert(!results2.all.exists(opmObjectMatches(_, d1)))
    assert(!results2.all.exists(opmObjectMatches(_, d2)))
    assert(results2.all.exists(opmObjectMatches(_, d3)))
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

    val results2 = search(_.name).equals("search_lte").search(_.description).not().lessThanOrEqual("1")
    assert(results2.all.length == 1)
    assert(opmObjectMatches(results2.all.head, d2))
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
    val obj2 = get(key).get
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
    val obj4 = get(key).get
    assert(obj4.timeline.size == 4)
    // One step back, both are set in the same diff
    assert(obj4.timeline.head.id == 2)
    // Two steps back, it meets the previous timeline
    assert(obj4.timeline.head.name == "name2")
    assert(obj4.timeline.tail == obj2.timeline)
  }

  test("update an Option via squashPut succeeds") {
    val d1 =
      OpmFactory.instance[TestDomain]("option-squashPut").
        set(_.maybeName).to(Some("a")).
        set(_.maybeLong).to(Some(100L))
    squashPut(d1)

    val d2 =
      OpmFactory.instance[TestDomain]("option-squashPut").
        set(_.maybeName).to(Some("b")).
        set(_.maybeLong).to(Some(200L))
    squashPut(d2)

    val d3 = get("option-squashPut").get
    assert(d3.maybeName == Some("b"))
    // scala compares boxed primitives using type coercion when == is used, so avoid that
    assert(d3.maybeLong.isDefined && !(d3.maybeLong.get equals 200))
    assert(d3.maybeLong.isDefined && (d3.maybeLong.get equals 200L))
  }

  test("primitive types work") {
    val d1 =
      OpmFactory.instance[TestDomain]("non-bson").
        set(_.count).to(1L).
        set(_.byte).to(66: Byte).
        set(_.char).to('a').
        set(_.short).to(23: Short).
        set(_.int).to(57).
        set(_.float).to(0.1F).
        set(_.double).to(0.2).
        set(_.maybeFloat).to(Some(0.3F)).
        prune
    put(d1)

    val d2 = get("non-bson").get

    assert(d2.count equals 1L)
    assert(d2.byte equals (66: Byte))
    assert(d2.char equals 'a')
    assert(d2.short equals (23: Short))
    assert(d2.int equals 57)
    assert(d2.float equals 0.1F)
    assert(d2.double equals 0.2)

    // Containers of some types don't work yet
    assert(d2.maybeFloat.isDefined && !(d2.maybeFloat.get equals 0.3F))
    assert(d2.maybeFloat.isDefined && d2.maybeFloat.get.isInstanceOf[Double])
  }

  test("update a non-String via squashPut succeeds") {
    val d1 =
      OpmFactory.instance[TestDomain]("option-squashPut").
        set(_.count).to(100)
    squashPut(d1)

    val d2 =
      OpmFactory.instance[TestDomain]("option-squashPut").
        set(_.count).to(200)
    squashPut(d2)

    val d3 = get("option-squashPut").get
    assert(d3.count == 200)
  }

  test("a loaded object with history returns empty options properly") {
    val d1 =
      OpmFactory.instance[TestDomain]("option-history").
        set(_.count).to(100).
        set(_.maybeLong).to(None).
        prune
    put(d1)

    val d2 = get("option-history")
    assert(d2.isDefined)
    assert(d2.get.count == 100)
    assert(d2.get.maybeLong == None)

    put(d2.get.set(_.description).to("test"))

    val d3 = get("option-history")
    assert(d3.isDefined)
    assert(d3.get.count == 100)
    assert(d3.get.description == "test")
    assert(d3.get.maybeLong == None)
  }

  test("setting a property from Some to None works properly") {
    val d1 = OpmFactory.instance[TestDomain]("option-set-to-none").
      set(_.maybeLong).to(Some(100L)).
      prune
    put(d1)

    val d2 = get("option-set-to-none")
    assert(d2.isDefined)
    assert(d2.get.maybeLong == Some(100L))

    put(d2.get.set(_.maybeLong).to(None))

    val d3 = get("option-set-to-none")
    assert(d3.isDefined)
    assert(d3.get.maybeLong == None)
  }

  test("evolve from trait succeeds in writing to Mongo") {
    import MongoTest.TestClass
    val d1 =
      OpmFactory.instance[TestClass]("test_class_1").
        set(_.id).to(1).
        prune

    TestClass.put(d1)

    val d2 =
      OpmFactory.instance[Named]().
        set(_.name).to("name1").
        prune

    val d3 = TestClass.get("test_class_1")
    assert(d3.map(_.id) === Some(1))

    val d4 = d3.get evolve d2

    TestClass.squashPut(d4)

    val d5 = TestClass.get("test_class_1")
    assert(d5.map(_.id) === Some(1))
    assert(d5.map(_.name) === Some("name1"))
    assert(d5.get.timeline.size === 2)
  }

  test("can audit changes") {
    import MongoTest.TestClassAudited
    val guid1 = GUID.randomGUID()
    val guid2 = GUID.randomGUID()
    val d1 =
      OpmFactory.instance[TestClassAudited]("test_class_audited_1").
        set(_.id).to(1).by(guid1, "testing1")

    TestClassAudited.put(d1)

    val d2 = TestClassAudited.get("test_class_audited_1").get.
      set(_.name).to("name1").
      set(_.id).to(2).by(guid2, "testing2")

    TestClassAudited.put(d2)

    val d3 = TestClassAudited.get("test_class_audited_1")
    assert(d3.map(_.id) === Some(2))
    assert(d3.map(_.name) === Some("name1"))
    assert(d3.get.timeline.size === 4)
    assert(d3.get.opmUpdatedBy === Some(guid2))
    assert(d3.get.opmUpdateReason === Some("testing2"))
    assert(d3.get.timeline.tail.head.opmUpdatedBy === None)
    assert(d3.get.timeline.tail.head.opmUpdateReason === None)
    assert(d3.get.timeline.tail.tail.head.opmUpdatedBy === Some(guid1))
    assert(d3.get.timeline.tail.tail.head.opmUpdateReason === Some("testing1"))
  }

  test("can audit changes with evolve") {
    import MongoTest.TestClassAudited
    val guid1 = GUID.randomGUID()
    val guid2 = GUID.randomGUID()
    val d1 =
      OpmFactory.instance[TestClassAudited]("test_class_audited_2").
        set(_.id).to(1).by(guid1, "testing1")

    TestClassAudited.put(d1)

    val d2 =
      OpmFactory.instance[NamedAudited]().
        set(_.name).to("name1").by(guid2, "testing2")

    val d3 = TestClassAudited.get("test_class_audited_2")
    assert(d3.map(_.id) === Some(1))

    val d4 = d3.get evolve d2

    TestClassAudited.put(d4)

    val d5 = TestClassAudited.get("test_class_audited_2")
    assert(d5.map(_.id) === Some(1))
    assert(d5.map(_.name) === Some("name1"))
    assert(d5.get.timeline.size === 3)
    assert(d5.get.opmUpdatedBy === Some(guid2))
    assert(d5.get.opmUpdateReason === Some("testing2"))
    assert(d5.get.timeline.tail.head.opmUpdatedBy === Some(guid1))
    assert(d5.get.timeline.tail.head.opmUpdateReason === Some("testing1"))
  }

  test("can audit squashPut") {
    import MongoTest.TestClassAudited
    val guid1 = GUID.randomGUID()
    val guid2 = GUID.randomGUID()
    val guid3 = GUID.randomGUID()
    val guid4 = GUID.randomGUID()
    val d1 =
      OpmFactory.instance[TestClassAudited]("test_class_audited_3").
        set(_.id).to(1).by(guid1, "testing1").
        set(_.name).to("name1").by(guid2, "testing2")

    TestClassAudited.squashPut(d1, guid3, "testing3")

    val d2 =
      OpmFactory.instance[TestClassAudited]("test_class_audited_3").
        set(_.id).to(2).by(guid1, "testing1").
        set(_.name).to("name2").by(guid2, "testing2")

    TestClassAudited.squashPut(d2)

    val d3 =
      OpmFactory.instance[TestClassAudited]("test_class_audited_3").
        set(_.id).to(3).by(guid1, "testing1").
        set(_.name).to("name3").by(guid2, "testing2")

    TestClassAudited.squashPut(d3, guid4, "testing4")

    val d4 = TestClassAudited.get("test_class_audited_3")
    assert(d4.map(_.id) === Some(3))
    assert(d4.map(_.name) === Some("name3"))
    assert(d4.get.timeline.size === 4)
    assert(d4.get.opmUpdatedBy === Some(guid4))
    assert(d4.get.opmUpdateReason === Some("testing4"))
    assert(d4.get.timeline.tail.head.id === 2)
    assert(d4.get.timeline.tail.head.name === "name2")
    assert(d4.get.timeline.tail.head.opmUpdatedBy === Some(guid2))
    assert(d4.get.timeline.tail.head.opmUpdateReason === Some("testing2"))
    assert(d4.get.timeline.tail.tail.head.id === 1)
    assert(d4.get.timeline.tail.tail.head.name === "name1")
    assert(d4.get.timeline.tail.tail.head.opmUpdatedBy === Some(guid3))
    assert(d4.get.timeline.tail.tail.head.opmUpdateReason === Some("testing3"))
  }

  test("pending persists to Mongo") {
    val key = uniqueKey
    val obj1 = instance[TestDomain](key).
      set(_.id).to(1).
      set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    put(obj1)
    val obj2 = get(key).get
    val caught1 = evaluating {
      obj2.name
    } should produce[RuntimeException]
    assert(caught1 match {
      case PendingOpmValueException(message) => true
      case e: NoSuchElementException => false
    })
    Thread.sleep(101)
    val caught2 = evaluating {
      obj2.name
    } should produce[RuntimeException]
    assert(caught2 match {
      case PendingOpmValueException(message) => false
      case e: NoSuchElementException => true
    })
  }

  test("set value after pending persists to Mongo") {
    val key = uniqueKey
    val obj1 = instance[TestDomain](key).
      set(_.id).to(1).
      set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    put(obj1)
    val obj2 = get(key).get
    val caught1 = evaluating {
      obj2.name
    } should produce[RuntimeException]
    assert(caught1 match {
      case PendingOpmValueException(message) => true
      case e: NoSuchElementException => false
    })
    val obj3 = obj2.set(_.name).to("name1")
    squashPut(obj3)
    val obj4 = get(key).get
    assert(obj4.name === "name1")
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
