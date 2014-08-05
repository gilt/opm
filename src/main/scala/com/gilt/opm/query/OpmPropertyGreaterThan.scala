package com.gilt.opm.query

import com.gilt.opm.utils.MongoHelper
import com.mongodb.casbah.commons.MongoDBObject
import MongoHelper.toMongo

/**
 * Case class representing the logic to filter a property that is greater than the given value.
 *
 * @param valueTranslator: see [[com.gilt.opm.query.OpmSearcher]]
 */
case class OpmPropertyGreaterThan[T <% Ordered[T]](property: String, value: T, valueTranslator: Option[(String, Any) => Any] = None) extends OpmPropertyQuery {
  override def isMatch(obj: Any): Boolean =
    if (obj == null) false
    else obj.asInstanceOf[T] > value

  override def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) =
    if (matchInverse) MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lte" -> toMongo(value, translate(property))))
    else MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gt" -> toMongo(value, translate(property))))
}
