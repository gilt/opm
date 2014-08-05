package com.gilt.opm.query

import com.mongodb.casbah.commons.MongoDBObject

/**
 * Case class representing the logic to filter a property that is blank in the datastore.
 */
case class OpmPropertyBlank(property: String) extends OpmPropertyQuery {
  override def isMatch(obj: Any) = obj == null

  val valueTranslator = None

  override def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) =
    if (matchInverse) MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$exists" -> true) )
    else MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$exists" -> false))
}
