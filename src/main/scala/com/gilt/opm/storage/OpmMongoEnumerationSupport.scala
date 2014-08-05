package com.gilt.opm.storage

import com.gilt.opm._
import com.mongodb.casbah.commons.Imports._

/**
 * Adds support for storing scala `Enumeration`s to MongoDB. You can mix in this trait on its own or
 * alongside any other support traits.
 */
trait OpmMongoEnumerationSupport extends MongoMapper {
  private val mapTo: OpmToMongoMapper = {
    case (_, _, v: Enumeration$Val) => {
      // inspired by Jackson's Scala module
      val parentEnum = v.getClass.getSuperclass.getDeclaredFields.find(_.getName == "$outer").get
      val enumClass = parentEnum.get(v).getClass.getName
      MongoDBObject("_ec" -> enumClass, "_ev" -> v.toString)
    }
  }
  abstract override def toMongoMapper: OpmToMongoMapper = mapTo orElse super.toMongoMapper

  private val mapFrom: OpmFromMongoMapper = {
    case (_, _, o: BasicDBObject) if o.contains("_ec") && o.contains("_ev") => {
      val enumClass = Class.forName(o("_ec").toString)
      val enumObject = enumClass.getField("MODULE$").get(null)
      val value = o("_ev").toString
      enumClass.getMethod("withName",classOf[String]).invoke(enumObject, value)
    }
  }
  abstract override def fromMongoMapper: OpmFromMongoMapper = mapFrom orElse super.fromMongoMapper
}
