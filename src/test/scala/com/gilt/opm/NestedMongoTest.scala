package com.gilt.opm

import org.scalatest.FunSuite
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.commons.MongoDBObject

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
  }

  trait OpmB extends B with OpmObject

  trait OpmC extends C with OpmObject

  case class BImpl(name: String, c: Option[C] = None) extends B

  case class CImpl(name: String) extends C
}

class NestedOpmMongoTest extends FunSuite with OpmMongoStorage {
  import NestedMongoTest._
  import OpmFactory._
  val collection = MongoConnection()("opm-MongoTest")("nested-opm")
  collection.drop()

  test("OpmObject nesting") {
    val c = instance[OpmC]("c").set(_.name).to("hello, C")
    val b = instance[OpmB]("b").set(_.name).to("hello, B").set(_.c).to(Some(c))
    val a = instance[OpmA]("a").set(_.id).to(3).set(_.b).to(Some(b))
    put(a)

    val loadedA = get[OpmA](a.key)
    assert(loadedA.get === a)
    assert(loadedA.get.b.get === b)
    assert(loadedA.get.b.get.c.get === c)
  }
}

class NestedNonMongoTest extends FunSuite with OpmMongoStorage {
  import NestedMongoTest._
  import OpmFactory._
  import com.mongodb.casbah._
  import com.mongodb.casbah.commons.MongoDBObject
  import com.mongodb.casbah.Implicits._
  import com.mongodb.casbah.commons.Implicits.wrapDBObj
  val collection = MongoConnection()("opm-MongoTest")("nested-non-opm")
  collection.drop()

  override def toMongoMapper: Option[PartialFunction[(String, AnyRef), AnyRef]] =
    Some {
      {
        case maybeB if maybeB._2.isInstanceOf[B] => {
          val b = maybeB._2.asInstanceOf[B]
          val fields = Seq(Some("name" -> b.name), b.c.map("c" -> _)).flatten
          MongoDBObject(fields:_*)
        }
      }
    }
  override def fromMongoMapper: Option[PartialFunction[(String, AnyRef), AnyRef]] = Some(
    {
      case maybeB if maybeB._1 == "b" =>
        val m = wrapDBObj(maybeB._2.asInstanceOf[DBObject])
        Some(
          BImpl(
            name = m.as[String]("name"),
            c = m.getAs[BasicDBList]("c").map(n => CImpl(name = n.get(0).asInstanceOf[String]))))
    }
  )

  test("OpmObject nesting") {
    val c = CImpl(name = "Hi, I'm a C.")
    val b = BImpl(name = "Hi, I'm a B.", c = Some(c))
    val a = instance[OpmA]("a").set(_.id).to(3).set(_.b).to(Some(b))

    put(a)

    val loadedA = get[OpmA](a.key)
    assert(loadedA.get === a)
    assert(loadedA.get.b.get === b)
    assert(loadedA.get.b.get.c.get === c)
  }

}

// todo: show how to use different OpmMongoStorage instances for different types
