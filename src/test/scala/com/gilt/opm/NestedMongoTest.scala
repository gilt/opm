package com.gilt.opm

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import com.mongodb.casbah.MongoClient
import java.util.concurrent.TimeUnit
import org.scalatest.Matchers

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/6/12 8:40 AM
 */
object NestedMongoTest {
  trait A {
    def id: Int
    def b: Option[B]

  }
  trait OpmA extends A with OpmObject

  trait B {
    def name: String
    def c: Option[C]
  }

  trait C {
    def name: String
    def d: D
  }

  trait D {
    def name: String
  }

  trait OpmB extends B with OpmObject

  trait OpmC extends C with OpmObject

  trait OpmD extends D with OpmObject

  case class BImpl(name: String, c: Option[C] = None) extends B

  case class CImpl(name: String, d: D) extends C

  case class DImpl(name: String) extends D

}

class NestedOpmMongoTest extends FunSuite with OpmMongoStorage[NestedMongoTest.OpmA] with CollectionHelper with BeforeAndAfterAll with Matchers {
  import NestedMongoTest._
  import OpmFactory._
  import CollectionHelper.databaseName

  val collectionName = "nested-opm"

  override def beforeAll() {
    MongoClient()(databaseName)("nested-opm-opmb").drop()
    MongoClient()(databaseName)("nested-opm-opmb-locks").drop()

    MongoClient()(databaseName)("nested-opm-opmc").drop()
    MongoClient()(databaseName)("nested-opm-opmc-locks").drop()

    MongoClient()(databaseName)("nested-opm-opmd").drop()
    MongoClient()(databaseName)("nested-opm-opmd-locks").drop()
  }

  private def getStorage[T <: OpmObject](implicit mf: Manifest[T]): Option[OpmMongoStorage[T]] = {
    Option(new OpmMongoStorage[T] {
      val collection = MongoClient()(databaseName)("nested-opm-%s".format(mf.runtimeClass.getSimpleName.toLowerCase))
      val locks = MongoClient()(databaseName)("nested-opm-%s-locks".format(mf.runtimeClass.getSimpleName.toLowerCase))
      override def nestedToStorage[U <: OpmObject](obj: Option[U])(implicit mf: Manifest[U]) = getStorage[U]
    })
  }
  override def nestedToStorage[T <: OpmObject](obj: Option[T] = None)(implicit mf: Manifest[T]) = getStorage[T]

  test("OpmObject nesting") {
    val d = instance[OpmD]("d").set(_.name).to("hello, D")
    val c = instance[OpmC]("c", Map("name" -> "hello, C", "d" -> d))
    val b = instance[OpmB]("b").set(_.name).to("hello, B").set(_.c).to(Some(c))
    val a = instance[OpmA]("a").set(_.id).to(3).set(_.b).to(Some(b))
    put(a)

    val loadedA = get(a.opmKey)
    assert(loadedA.get === a)
    assert(loadedA.get.b.get === b)
    assert(loadedA.get.b.get.c.get === c)
    assert(loadedA.get.b.get.c.get.d === d)
  }

  test("OpmObject optional nesting with changes") {
    val d = instance[OpmD]("d-optional-changed").set(_.name).to("hello, D")
    val c = instance[OpmC]("c-optional-changed", Map("name" -> "hello, C", "d" -> d))
    val b = instance[OpmB]("b-optional-changed").set(_.name).to("hello, B").set(_.c).to(Some(c))
    val a = instance[OpmA]("a-optional-changed").set(_.id).to(3).set(_.b).to(Some(b))
    put(a)

    val loadedA = get(a.opmKey)
    val changedB = loadedA.get.b.get.asInstanceOf[OpmB].set(_.name).to("changed")
    val changedA = loadedA.get.set(_.b).to(Some(changedB))
    put(changedA)

    val loadedChangedA = get(a.opmKey)
    assert(loadedChangedA.get === changedA)
    assert(loadedChangedA.get.b.get === changedB)
  }

  test("OpmObject nesting with pending") {
    val d = instance[OpmD]("d-pending").set(_.name).toPending(1000, TimeUnit.MILLISECONDS)
    val c = instance[OpmC]("c-pending", Map("name" -> "hello, C", "d" -> d))
    val b = instance[OpmB]("b-pending").set(_.name).to("hello, B").set(_.c).to(Some(c))
    val a = instance[OpmA]("a-pending").set(_.id).to(3).set(_.b).to(Some(b))
    put(a)

    val loadedA = get(a.opmKey)
    assert(loadedA.get === a)
    assert(loadedA.get.id === 3)
    assert(loadedA.get.b.get === b)
    assert(loadedA.get.b.get.name === "hello, B")
    assert(loadedA.get.b.get.c.get === c)
    assert(loadedA.get.b.get.c.get.name === "hello, C")
    assert(loadedA.get.b.get.c.get.d === d)
    an [PendingOpmValueException] should be thrownBy {
      loadedA.get.b.get.c.get.d.name
    }
    assert(loadedA.get.isPending(_.b.get.c.get.d.name))
    Thread.sleep(1000)
    an [NoSuchElementException] should be thrownBy {
      loadedA.get.b.get.c.get.d.name
    }
    assert(!loadedA.get.isPending(_.b.get.c.get.d.name))
  }
}

class NestedNonMongoTest extends FunSuite with OpmMongoStorage[NestedMongoTest.OpmA] with CollectionHelper {
  import NestedMongoTest._
  import OpmFactory._
  import com.mongodb.casbah.Imports._
  import com.mongodb.casbah.commons.MongoDBObject
  import com.mongodb.casbah.Implicits._
  import com.mongodb.casbah.commons.Implicits.wrapDBObj

  override val collectionName = "nested-non-opm"

  override def toMongoMapper: OpmToMongoMapper = {
    case maybeB if maybeB._3.isInstanceOf[B] => {
      val b = maybeB._3.asInstanceOf[B]
      val fields = Seq(Some("name" -> b.name), b.c.map(
        c => "c" -> MongoDBObject("name" -> c.name, "d" -> MongoDBObject("name" -> c.d.name)))).flatten
      MongoDBObject(fields:_*)
    }
  }

  override def fromMongoMapper: OpmFromMongoMapper = {
    case maybeB if maybeB._1 == "b" =>
      val m = wrapDBObj(maybeB._3.asInstanceOf[DBObject])
        BImpl(
          name = m.as[String]("name"),
          c = {
            val c = m.getAs[BasicDBObject]("c")
            c.map {
              cObj => CImpl(name = cObj.get("name").toString, d = {
                val dObj = cObj.get("d").asInstanceOf[BasicDBObject]
                DImpl(name = dObj.get("name").toString)
              })
            }
          }
        )
    }

  test("non-OpmObject nesting") {
    val d = DImpl(name = "Hi, I'm a D.")
    val c = CImpl(name = "Hi, I'm a C.", d = d)
    val b = BImpl(name = "Hi, I'm a B.", c = Some(c))
    val a = instance[OpmA]("a").set(_.id).to(3).set(_.b).to(Some(b))

    put(a)

    val loadedA = get(a.opmKey)
    assert(loadedA.get === a)
    assert(loadedA.get.b.get === b)
    assert(loadedA.get.b.get.c.get === c)
    assert(loadedA.get.b.get.c.get.d === d)
  }

}

// todo: show how to use different OpmMongoStorage instances for different types
