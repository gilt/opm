package com.gilt.opm.query

import com.mongodb.casbah.commons.MongoDBObject
import com.gilt.opm.OpmMongoStorage

/**
 * To be used when no filter is required.
 */
object OpmQueryNoFilter extends OpmPropertyQuery{
  val property = ""
  def isMatch(obj: Any) = true
  val valueTranslator = None
  def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) = MongoDBObject(OpmMongoStorage.Key -> MongoDBObject("$exists" -> true))
}
