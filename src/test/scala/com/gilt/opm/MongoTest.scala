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
class MongoTest extends FunSuite with OpmMongoStorage[Int, MongoTest.TestDomain] {
  import MongoTest._
  import OpmFactory._
  val collection = MongoConnection()("opm-MongoTest")("opm")
  collection.drop()

  test("write 1000") {
    val count = 1000

    val obj = (1 to count).foldLeft(instance[TestDomain]) {
      case (i: TestDomain, n: Int) =>
        i.set(_.id).to(n)
    }
    assert(obj.timeline.size === count + 1)
    create(1, obj)

    val loaded = retrieve(1)
    assert(loaded.get.id === count.toLong)
    val history = loaded.get.timeline
    assert(history.init.size === count)
    history.init.zipWithIndex.foreach(t => assert(t._1.id + t._2 === count, "%s -> %d".format(t, t._1.id + t._2)))

    delete(1)
    val obj2 = history.drop(count/2).head
    create(1, obj2)
    val reloaded = retrieve(1)
    assert(reloaded.get.timeline.size === obj2.timeline.size)
    assert(reloaded.get.id === count/2.toLong)
    val historyReloaded = reloaded.get.timeline
    historyReloaded.zip(obj2.timeline).foreach(x => assert(x._1 === x._2))
  }

  test("phase != 0 write ordering works") {
    val obj = instance[TestDomain].set(_.id).to(1).set(_.id).to(2).set(_.id).to(3).set(_.id).to(4)
    assert(obj.timeline.size === 5)

    create(2, obj)
    val obj2 = obj.set(_.id).to(5).set(_.id).to(6)
    update(2, obj2)
    val obj3 = retrieve(2).get
    println(obj3.timeline.mkString("\n"))
    for (i <- 6 to 1 by -1) {
      assert(obj3.timeline.drop(6 - i).head.id === i)
    }
  }
}
