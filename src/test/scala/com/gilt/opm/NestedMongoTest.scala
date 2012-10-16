package com.gilt.opm

import org.scalatest.FunSuite
import com.mongodb.casbah.MongoConnection

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

class NestedOpmMongoTest extends FunSuite with OpmMongoStorage {
  import NestedMongoTest._
  import OpmFactory._
  val collection = MongoConnection()("opm-MongoTest")("nested-opm")
  collection.drop()

  test("OpmObject nesting") {
    val d = instance[OpmD]("d").set(_.name).to("hello, D")
    val c = instance[OpmC]("c", Map("name" -> "hello, C", "d" -> d))
    val b = instance[OpmB]("b").set(_.name).to("hello, B").set(_.c).to(Some(c))
    val a = instance[OpmA]("a").set(_.id).to(3).set(_.b).to(Some(b))
    put(a)

    val loadedA = get[OpmA](a.opmKey)
    assert(loadedA.get === a)
    assert(loadedA.get.b.get === b)
    assert(loadedA.get.b.get.c.get === c)
    assert(loadedA.get.b.get.c.get.d === d)
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

  override def toMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] =
    Some {
      {
        case maybeB if maybeB._3.isInstanceOf[B] => {
          val b = maybeB._3.asInstanceOf[B]
          val fields = Seq(Some("name" -> b.name), b.c.map(
            c => "c" -> MongoDBObject("name" -> c.name, "d" -> MongoDBObject("name" -> c.d.name)))).flatten
          MongoDBObject(fields:_*)
        }
      }
    }
  override def fromMongoMapper: Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]] = Some(
    {
      case maybeB if maybeB._1 == "b" =>
        val m = wrapDBObj(maybeB._3.asInstanceOf[DBObject])
          BImpl(
            name = m.as[String]("name"),
            {
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
  )

  test("non-OpmObject nesting") {
    val d = DImpl(name = "Hi, I'm a D.")
    val c = CImpl(name = "Hi, I'm a C.", d)
    val b = BImpl(name = "Hi, I'm a B.", c = Some(c))
    val a = instance[OpmA]("a").set(_.id).to(3).set(_.b).to(Some(b))

    put(a)

    val loadedA = get[OpmA](a.opmKey)
    assert(loadedA.get === a)
    assert(loadedA.get.b.get === b)
    assert(loadedA.get.b.get.c.get === c)
    assert(loadedA.get.b.get.c.get.d === d)
  }

}

// todo: show how to use different OpmMongoStorage instances for different types
