package com.gilt.opm

import java.util.Date
import org.scalatest.FunSuite
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.commons.{MongoDBObject, MongoDBList}
import com.mongodb.{DBObject, BasicDBList}
import com.mongodb.casbah.commons.Implicits._
import java.util.concurrent.TimeUnit

/**
 * Testing suite for OpmMongoStorage with custom toMongoMapper and fromMongoMapper
 *
 * @author: Ryan Martin
 * @since: 10/31/12 10:22 AM
 */
object MongoTestWithCustomMappers {
  trait TestDomain extends OpmObject {
    def id: Long
    def name: String
    def createdAt: Date
    def updatedAt: Option[Date]
    def tags: Seq[String]
    def startMonth: Month
    def endMonth: Option[Month]
  }

  case class Month(month: Int, year: Int)
}

class MongoTestWithCustomMappers extends FunSuite with OpmMongoStorage[MongoTestWithCustomMappers.TestDomain] with CollectionHelper {
  import MongoTestWithCustomMappers._
  import OpmFactory._
  override val collectionName = "opm_custom_mappers"

  override def toMongoMapper: OpmToMongoMapper = {
    case ("id", _, str) => str.asInstanceOf[Long].asInstanceOf[AnyRef]
    case ("createdAt", _, ts) => ts.asInstanceOf[Date].getTime.asInstanceOf[AnyRef]
    case ("updatedAt", _, ts) => ts.asInstanceOf[Option[Date]].map(_.getTime.asInstanceOf[AnyRef])
    case ("startMonth", _, Month(month, year)) => MongoDBObject("month" -> month, "year" -> year)
    case ("endMonth", _, Month(month, year)) => MongoDBObject("month" -> month, "year" -> year)
    case ("name", _, str) => str
    case ("tags", _, set) => {
      val b = MongoDBList.newBuilder
      set.asInstanceOf[Seq[_]].foreach(k => b += k)
      b.result()
    }
  }

  def m(obj: Any): MongoDBObject = wrapDBObj(obj.asInstanceOf[DBObject])
  override def fromMongoMapper: OpmFromMongoMapper = {
    case ("id", _, str) => str.asInstanceOf[java.lang.Long]
    case ("createdAt", _, str) => new Date(str.asInstanceOf[Long])
    case ("updatedAt", _, str) => new Date(str.asInstanceOf[Long])
    case ("startMonth", _, dbObj) => Month(month = m(dbObj).as[Int]("month"), year = m(dbObj).as[Int]("year"))
    case ("endMonth", _, dbObj) => Month(month = m(dbObj).as[Int]("month"), year = m(dbObj).as[Int]("year"))
    case ("name", _, str) => str
    case ("tags", _, dbObj) => {
      val list = dbObj.asInstanceOf[BasicDBList]
      Seq[String]() ++ list.toArray.map(_.toString)
    }
  }

  test("to/fromMongoMapper works for lists") {
    val d1 = OpmFactory.instance[TestDomain]("mongo_test_with_custom_mappers1").set(_.tags).to(Seq("tag1"))
    put(d1)

    val d2 = get("mongo_test_with_custom_mappers1").get
    assert(d1.tags == Seq("tag1"))
    assert(d2.tags == d1.tags)

    val d3 = OpmFactory.instance[TestDomain]("mongo_test_with_custom_mappers1").set(_.tags).to(Seq("tag1", "tag2"))
    put(d3)

    val d4 = get("mongo_test_with_custom_mappers1").get
    assert(d4.tags != d1.tags)
    assert(d4.tags == Seq("tag1", "tag2"))
    assert(d4.tags == d3.tags)
  }

  test("to/fromMongoMapper works for Option") {
    val now = new Date
    val d1 = OpmFactory.instance[TestDomain]("mongo_test_with_custom_mappers2").set(_.createdAt).to(now).set(_.updatedAt).to(None)
    put(d1)

    val d2 = get("mongo_test_with_custom_mappers2").get
    assert(d1.createdAt == now)
    assert(d2.createdAt == d1.createdAt)
    assert(d2.updatedAt == None)
    assert(d2.updatedAt == d1.updatedAt)

    val tomorrow = new Date(now.getTime + TimeUnit.DAYS.toMillis(1))
    val d3 = OpmFactory.instance[TestDomain]("mongo_test_with_custom_mappers2").set(_.createdAt).to(tomorrow).set(_.updatedAt).to(Option(tomorrow))
    put(d3)

    val d4 = get("mongo_test_with_custom_mappers2").get
    assert(d4.createdAt != d1.createdAt)
    assert(d4.updatedAt != d1.updatedAt)
    assert(d4.createdAt == tomorrow)
    assert(d4.updatedAt.get == tomorrow)
    assert(d4.createdAt == d3.createdAt)
    assert(d4.updatedAt == d3.updatedAt)
  }

  test("to/fromMongoMapper works for custom type") {
    val d1 = OpmFactory.instance[TestDomain]("mongo_test_with_custom_mappers3").set(_.startMonth).to(Month(1, 2012)).set(_.endMonth).to(None)
    put(d1)

    val d2 = get("mongo_test_with_custom_mappers3").get
    assert(d1.startMonth == Month(1, 2012))
    assert(d2.startMonth == d1.startMonth)
    assert(d2.endMonth == None)
    assert(d2.endMonth == d1.endMonth)

    val d3 = OpmFactory.instance[TestDomain]("mongo_test_with_custom_mappers3").set(_.startMonth).to(Month(2, 2012)).set(_.endMonth).to(Option(Month(3, 2012)))
    put(d3)

    val d4 = get("mongo_test_with_custom_mappers3").get
    assert(d4.startMonth != d1.startMonth)
    assert(d4.endMonth != d1.endMonth)
    assert(d4.startMonth == Month(2, 2012))
    assert(d4.endMonth.get == Month(3, 2012))
    assert(d4.startMonth == d3.startMonth)
    assert(d4.endMonth == d3.endMonth)
  }
}
