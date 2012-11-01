package com.gilt.opm.query

import com.mongodb.casbah.commons.MongoDBObject

/**
 * Case class representing the logic to filter a property that is blank in the datastore.
 *
 * @author: Ryan Martin
 * @since: 11/2/12 8:51 AM
 */
case class OpmPropertyBlank(property: String) extends OpmPropertyQuery {
  override def isMatch(obj: Any) = obj == null
  override def toMongoDBObject(prefix: String = "") = MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$exists" -> false) )
}
