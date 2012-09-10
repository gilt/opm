package com.gilt.opm

import org.scalatest.FunSuite
import org.specs2.matcher.ShouldMatchers
import com.mongodb.casbah.MongoConnection

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/6/12 8:40 AM
 */
object NestedMongoTest {
  trait A extends OpmObject {
    def id: Int
    def b: B
  }

  trait B extends OpmObject {
    def name: String
  }
}

class NestedMongoTest extends FunSuite with OpmMongoStorage[NestedMongoTest.A] {
  import NestedMongoTest._
  import OpmFactory._
  val collection = MongoConnection()("opm-MongoTest")("nested")
  collection.drop()

//  test("OpmObject nesting") {
//    val b = instance[B]("b").set(_.name).to("hello, B")
//    val a = instance[A]("a").set(_.id).to(3).set(_.b).to(b)
//    put(a)
//  }
}
