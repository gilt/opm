package com.giltgroupe.service.commons.mongo

import com.giltgroupe.util.Timestamp

/**
 * A library of methods to help with marshalling/unmarshalling objects to/from Mongo.
 *
 * @author: Ryan Martin
 * @since: 11/7/12 7:28 PM
 */
object MongoHelper {
  def toMongo(v: Any): Any = {
    v match {
      case value: Timestamp => value.getTime
      case _ => v
    }
  }
}
