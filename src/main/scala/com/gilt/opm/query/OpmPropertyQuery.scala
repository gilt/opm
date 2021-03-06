package com.gilt.opm.query

import com.mongodb.DBObject

/**
 * Implement this with case classes to define the manner in which individual properties can be queried.
 */
trait OpmPropertyQuery {
  /**
   * The case-sensitive string that corresponds to the property of the object you are querying.
   */
  val property: String

  def isMatch(obj: Any): Boolean

  // @see com.gilt.opm.query.OpmSearcher
  def valueTranslator: Option[(String, Any) => Any]

  // A convenience method to translate a value for a given field.
  def translate(field: String) = valueTranslator.map(f => (v: Any) => f(field, v))
  /**
   * Define this to determine how the query can be used by Mongo.
   *
   * @param prefix: The prefix to add to the property name in the query.
   * @param matchInverse: Match the inverse of the query, i.e. 'not'.
   * @return
   */
  def toMongoDBObject(prefix: String = "", matchInverse: Boolean = false): DBObject
}
