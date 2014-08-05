package com.gilt.opm.query

import com.gilt.opm.utils.MongoHelper
import com.mongodb.casbah.commons.{MongoDBList, MongoDBObject}
import MongoHelper._

/**
 * Case class representing the logic to filter a property that is included the given array.
 *
 * @param valueTranslator: see [[com.gilt.opm.query.OpmSearcher]]
 */
case class OpmPropertyIn(property: String, value: Iterable[Any], valueTranslator: Option[(String, Any) => Any] = None) extends OpmPropertyQuery {
  override def isMatch(obj: Any) = value.exists(_ == obj)

  override def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false) =
    if (matchInverse) MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$nin" -> MongoDBList(value.map(toMongo(_, translate(property))).toSeq: _*)))
    else MongoDBObject("%s%s".format(prefix, property) -> MongoDBObject("$in" -> MongoDBList(value.map(toMongo(_, translate(property))).toSeq: _*)))
}
