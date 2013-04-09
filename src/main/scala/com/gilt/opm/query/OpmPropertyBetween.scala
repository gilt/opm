package com.gilt.opm.query

import com.mongodb.casbah.commons.{MongoDBList, MongoDBObject}
import com.giltgroupe.service.commons.mongo.MongoHelper.toMongo

/**
 * Case class representing the logic to filter a property that is between the given values, inclusive.
 *
 * @param valueTranslator: @see com.gilt.opm.query.OpmSearcher
 *
 * @author: Ryan Martin
 * @since: 11/6/12 1:24 PM
 */
case class OpmPropertyBetween[T <% Ordered[T]](property: String, start: T, end: T, valueTranslator: Option[(String, Any) => Any] = None) extends OpmPropertyQuery {
  override def isMatch(obj: Any): Boolean = {
    if (obj == null) return false
    val curObj = obj.asInstanceOf[T]
    (curObj >= start) && (curObj <= end)
  }
  override def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) =
    if (matchInverse)
      MongoDBObject("$or" -> MongoDBList(
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lt" -> toMongo(start, translate(property)))),
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gt" -> toMongo(end, translate(property))))
      ))
    else
      MongoDBObject("$and" -> MongoDBList(
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gte" -> toMongo(start, translate(property)))),
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lte" -> toMongo(end, translate(property))))
      ))
}
