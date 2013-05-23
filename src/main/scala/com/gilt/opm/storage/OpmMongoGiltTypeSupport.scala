package com.gilt.opm.storage

import com.gilt.opm._
import com.giltgroupe.util.{CompactGuid, Timestamp}
import com.mongodb.casbah.commons.Imports._

/**
 * Adds support for storing {@link CompactGuid} and {@link Timestamp} members of an
 * {@link OpmObject} to MongoDB. You can mix in this trait on its own or alongside any other support
 * traits.
 */
trait OpmMongoGiltTypeSupport[T <: OpmObject] extends OpmMongoStorage[T] {
  override def toMongoMapper: OpmToMongoMapper = Some {
    case (_, _, g: CompactGuid[_]) => MongoDBObject("_guid" -> g.toString)

    case (_, _, t: Timestamp) => MongoDBObject("_ts" -> t.getTime)

    case args if super.toMongoMapper.isDefined && super.toMongoMapper.get.isDefinedAt(args) => {
      super.toMongoMapper.get.apply(args)
    }
  }

  override def fromMongoMapper: OpmFromMongoMapper = Some {
    case (_, _, o: BasicDBObject) if o.contains("_guid") => CompactGuid[AnyRef](o("_guid").toString)

    case (_, _, o: BasicDBObject) if o.contains("_ts") && o("_ts").isInstanceOf[Number] =>
      new Timestamp(o("_ts").asInstanceOf[Number].longValue)

    case args if super.fromMongoMapper.isDefined && super.fromMongoMapper.get.isDefinedAt(args) => {
      super.fromMongoMapper.get.apply(args)
    }
  }
}
