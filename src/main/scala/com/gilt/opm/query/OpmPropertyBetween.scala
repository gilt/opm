package com.gilt.opm.query

import com.mongodb.casbah.commons.{MongoDBList, MongoDBObject}
import com.giltgroupe.service.commons.mongo.MongoHelper.toMongo

/**
 * Case class representing the logic to filter a property that is between the given values, inclusive.
 *
 * @author: Ryan Martin
 * @since: 11/6/12 1:24 PM
 */
case class OpmPropertyBetween[T <% Ordered[T]](property: String, start: T, end: T) extends OpmPropertyQuery {
  override def isMatch(obj: Any): Boolean = {
    val curObj = obj.asInstanceOf[T]
    (curObj >= start) && (curObj <= end)
  }
  override def toMongoDBObject(prefix: String = "") = MongoDBObject("$and" -> MongoDBList(
    MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gte" -> toMongo(start))),
    MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lte" -> toMongo(end)))
  ))
}
