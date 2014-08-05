package com.gilt.opm.storage

import com.gilt.gfc.id.Guid
import com.gilt.gfc.time.Timestamp
import com.gilt.opm._
import com.mongodb.casbah.commons.Imports._

/**
 * Adds support for storing `Guid` and `Timestamp` members of an
 * `OpmObject` to MongoDB. You can mix in this trait on its own or alongside any other support
 * traits.
 */
trait OpmMongoGiltTypeSupport extends MongoMapper {
  private val mapTo: OpmToMongoMapper = {
    case (_, _, g: Guid[_]) => MongoDBObject("_guid" -> g.toString)

    case (_, _, t: Timestamp) => MongoDBObject("_ts" -> t.getTime)
  }
  abstract override def toMongoMapper: OpmToMongoMapper = mapTo orElse super.toMongoMapper

  private val mapFrom: OpmFromMongoMapper = {
    case (_, _, o: BasicDBObject) if o.contains("_guid") => Guid[AnyRef](o("_guid").toString)

    case (_, _, o: BasicDBObject) if o.contains("_ts") && o("_ts").isInstanceOf[Number] =>
      new Timestamp(o("_ts").asInstanceOf[Number].longValue)
  }
  abstract override def fromMongoMapper: OpmFromMongoMapper = mapFrom orElse super.fromMongoMapper
}
