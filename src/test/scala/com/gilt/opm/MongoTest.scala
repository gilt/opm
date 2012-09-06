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
    create(obj)

    val loaded = retrieve(key)
    assert(loaded.get.id === count.toLong)
    val history = loaded.get.timeline
    assert(history.init.size === count)
    history.init.zipWithIndex.foreach(t => assert(t._1.id + t._2 === count, "%s -> %d".format(t, t._1.id + t._2)))

    delete(key)
    val obj2 = history.drop(count/2).head
    create(obj2)
    val reloaded = retrieve(1.toString)
    assert(reloaded.get.timeline.size === obj2.timeline.size)
    assert(reloaded.get.id === count/2.toLong)
    val historyReloaded = reloaded.get.timeline
    historyReloaded.zip(obj2.timeline).foreach(x => assert(x._1 === x._2))
  }

  test("phase != 0 write ordering works") {
    val key = 2.toString
    val obj = instance[TestDomain](key).set(_.id).to(1).set(_.id).to(2).set(_.id).to(3).set(_.id).to(4)
    assert(obj.timeline.size === 5)

    create(obj)
    val obj2 = obj.set(_.id).to(5).set(_.id).to(6)
    update(obj2)
    val obj3 = retrieve(key).get
    for (i <- 6 to 1 by -1) {
      assert(obj3.timeline.drop(6 - i).head.id === i)
    }
  }
}
