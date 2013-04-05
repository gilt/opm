package com.gilt.opm.query

import com.mongodb.casbah.commons.MongoDBObject
import com.giltgroupe.service.commons.mongo.MongoHelper.toMongo

/**
 * Case class representing the logic to filter a property that is equal to the given value.
 *
 * @param valueTranslator: @see com.gilt.opm.query.OpmSearcher
 *
 * @author: Ryan Martin
 * @since: 11/1/12 6:48 PM
 */
case class OpmPropertyEquals(property: String, value: Any, valueTranslator: Option[(String, Any) => Any] = None) extends OpmPropertyQuery {
  override def isMatch(obj: Any) = obj == value
  override def toMongoDBObject(prefix: String = "") = MongoDBObject("%s%s".format(prefix, property) -> toMongo(value, translate(property)))
}