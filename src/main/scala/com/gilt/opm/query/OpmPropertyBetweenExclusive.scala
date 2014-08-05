package com.gilt.opm.query

import com.gilt.opm.utils.MongoHelper
import com.mongodb.casbah.commons.{MongoDBList, MongoDBObject}
import MongoHelper.toMongo

/**
 * Case class representing the logic to filter a property that is between the given values, exclusive.
 *
 * @param valueTranslator: see [[com.gilt.opm.query.OpmSearcher]]
 */
case class OpmPropertyBetweenExclusive[T <% Ordered[T]](property: String, start: T, end: T, valueTranslator: Option[(String, Any) => Any] = None) extends OpmPropertyQuery {
  override def isMatch(obj: Any): Boolean = {
    if (obj == null) return false
    val curObj = obj.asInstanceOf[T]
    (curObj > start) && (curObj < end)
  }
  override def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) =
    if (matchInverse)
      MongoDBObject("$or" -> MongoDBList(
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lte" -> toMongo(start, translate(property)))),
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gte" -> toMongo(end, translate(property))))
      ))
    else
      MongoDBObject("$and" -> MongoDBList(
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gt" -> toMongo(start, translate(property)))),
        MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lt" -> toMongo(end, translate(property))))
      ))
}
