package com.gilt.opm.query

import com.mongodb.casbah.commons.{MongoDBList, MongoDBObject}

/**
 * Case class representing the logic to filter a property that is between the given values, exclusive.
 *
 * @author: Ryan Martin
 * @since: 11/6/12 1:24 PM
 */
case class OpmPropertyBetweenExclusive[T <% Ordered[T]](property: String, start: T, end: T) extends OpmPropertyQuery {
  override def isMatch(obj: Any): Boolean = {
    val curObj = obj.asInstanceOf[T]
    (curObj > start) && (curObj < end)
  }
  override def toMongoDBObject(prefix: String = "") = MongoDBObject("$and" -> MongoDBList(
    MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$gt" -> start)),
    MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$lt" -> end))
  ))
}
