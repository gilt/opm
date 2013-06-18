package com.gilt.opm.storage

import com.gilt.opm._
import com.mongodb.casbah.commons.Imports._
import java.net.InetAddress

/**
 * Adds support for storing {@link InetAddress}, {@link BigDecimal} and {@link BigInt} members of an
 * {@link OpmObject} to MongoDB. You can mix in this trait on its own or alongside any other support
 * traits.
 */
trait OpmMongoBasicTypeSupport extends MongoMapper {
  private val mapTo: OpmToMongoMapper = {
    case (_, _, a: InetAddress) => MongoDBObject("_inet" -> a.getHostAddress)

    case (_, _, b: BigDecimal) => MongoDBObject("_bigd" -> b.toString)

    case (_, _, b: BigInt) => MongoDBObject("_bigi" -> b.toString)
  }
  abstract override def toMongoMapper: OpmToMongoMapper = mapTo orElse super.toMongoMapper

  private val mapFrom: OpmFromMongoMapper = {
    case (_, _, o: BasicDBObject) if o.contains("_inet") => InetAddress.getByName(o("_inet").toString)

    case (_, _, o: BasicDBObject) if o.contains("_bigd") => BigDecimal(o("_bigd").toString)

    case (_, _, o: BasicDBObject) if o.contains("_bigi") => BigInt(o("_bigi").toString)
  }
  abstract override def fromMongoMapper: OpmFromMongoMapper = mapFrom orElse super.fromMongoMapper
}
