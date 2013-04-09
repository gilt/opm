package com.gilt.opm.query

import com.mongodb.casbah.commons.MongoDBObject
import com.giltgroupe.service.commons.mongo.MongoHelper.toMongo

/**
 * Case class representing the logic to filter a property that is greater than the given value.
 *
 * @param valueTranslator: @see com.gilt.opm.query.OpmSearcher
 *
 * @author: Ryan Martin
 * @since: 11/6/12 1:24 PM
 */
case class OpmPropertyGreaterThan[T <% Ordered[T]](property: String, value: T, valueTranslator: Option[(String, Any) => Any] = None) extends OpmPropertyQuery {
  override def isMatch(obj: Any): Boolean =
    if (obj == null) return false
    else obj.asInstanceOf[T] > value

  override def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) =
    if (matchInverse) MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lte" -> toMongo(value, translate(property))))
    else MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gt" -> toMongo(value, translate(property))))
}
