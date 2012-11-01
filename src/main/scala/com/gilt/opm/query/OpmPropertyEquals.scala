package com.gilt.opm.query

import com.mongodb.casbah.commons.MongoDBObject

/**
 * Case class representing the logic to filter a property that is equal to the given value.
 *
 * @author: Ryan Martin
 * @since: 11/1/12 6:48 PM
 */
case class OpmPropertyEquals(property: String, value: Any) extends OpmPropertyQuery {
  override def isMatch(obj: Any) = obj == value
  override def toMongoDBObject(prefix: String = "") = MongoDBObject("%s%s".format(prefix, property) -> value)
}