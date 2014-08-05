package com.gilt.opm.utils

import com.gilt.gfc.time.Timestamp
import com.gilt.opm.NanoTimestamp

/**
 * A library of methods to help with marshalling/unmarshalling objects to/from Mongo.
 */
object MongoHelper {
  def toMongo(v: Any, custom: Option[(Any) => Any] = None): Any = {
    v match {
      case ts: NanoTimestamp => ts.time
      case ts: Timestamp => new NanoTimestamp(ts).time
      case _ => custom.map(f => f(v)).getOrElse(v)
    }
  }
}
