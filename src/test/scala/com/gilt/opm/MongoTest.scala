package com.gilt.opm

import org.scalatest.FunSuite
import com.mongodb.casbah.MongoConnection
import java.util.Date

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
    def createdAt: Date
    def tags: Seq[String]
  }

  trait SimpleDomain extends OpmObject {
    def name: String
  }
}

class MongoTest extends FunSuite with OpmMongoStorage {
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

    val loaded = get[TestDomain](key)
    assert(loaded.get.id === count.toLong)
    val history = loaded.get.timeline
    assert(history.init.size === count)
    history.init.zipWithIndex.foreach(t => assert(t._1.id + t._2 === count, "%s -> %d".format(t, t._1.id + t._2)))

    remove(key)
    val obj2 = history.drop(count/2).head
    put(obj2)
    val reloaded = get[TestDomain](key)
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
    val obj3 = get[TestDomain](key).get
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

    put(d1)
    put(d2)

    val d3 = get[SimpleDomain]("sd1")
    assert(d3.map(_.name) === Some("d2"))
  }

  test("default to/from MongoMapper works for lists") {
    val d1 =
      OpmFactory.instance[TestDomain]("td1").
        set(_.name).to("name1").
        set(_.tags).to(Seq("tag1"))
    put(d1)

    val d2 = get[TestDomain]("td1").get
    assert(d1.tags == Seq("tag1"))
    assert(d2.tags == d1.tags)

    val d3 =
      OpmFactory.instance[TestDomain]("td1").
        set(_.name).to("name1").
        set(_.tags).to(Seq("tag1", "tag2"))
    put(d3)

    val d4 = get[TestDomain]("td1").get
    assert(d4.tags != d1.tags)
    assert(d4.tags == Seq("tag1", "tag2"))
    assert(d4.tags == d3.tags)
  }

  // Testing this here because it requires an implementation of get and put
  test("squashPut succeeds") {
    val obj1 = instance[TestDomain]("td_squash").
      set(_.id).to(1).
      set(_.name).to("name1")
    put(obj1)

    // Straight put, without a prune
    val obj2 = toSetter(get[TestDomain]("td_squash").get)
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

    val obj3 = instance[TestDomain]("td_squash").
      set(_.id).to(2).
      set(_.name).to("name2")
    squashPut(obj3)

    // With a squashed put
    val obj4 = toSetter(get[TestDomain]("td_squash").get)
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
}
