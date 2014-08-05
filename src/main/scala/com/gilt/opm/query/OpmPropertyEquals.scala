package com.gilt.opm.query

import com.gilt.opm.utils.MongoHelper
import com.mongodb.casbah.commons.MongoDBObject
import MongoHelper.toMongo

/**
 * Case class representing the logic to filter a property that is equal to the given value.
 *
 * @param valueTranslator: see [[com.gilt.opm.query.OpmSearcher]]
 */
case class OpmPropertyEquals(property: String, value: Any, valueTranslator: Option[(String, Any) => Any] = None) extends OpmPropertyQuery {
  override def isMatch(obj: Any) = obj == value

  override def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) =
    if (matchInverse) MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$ne" -> toMongo(value, translate(property))))
    else MongoDBObject("%s%s".format(prefix, property) -> toMongo(value, translate(property)))
}