package com.gilt.opm.storage

import com.gilt.opm._
import com.mongodb.casbah.commons._

/**
 * Adds support for storing {@link Enumeration}s to MongoDB. You can mix in this trait on its own or
 * alongside any other support traits.
 */
trait OpmMongoEnumerationSupport[T <: OpmObject] extends OpmMongoStorage[T] {
  override def toMongoMapper: OpmToMongoMapper = Some {
    case (_, _, v: scala.Enumeration$Val) => {
      // inspired by Jackson's Scala module
      val parentEnum = v.getClass.getSuperclass.getDeclaredFields.find(_.getName == "$outer").get
      val enumClass = parentEnum.get(v).getClass.getName
      MongoDBObject("_ec" -> enumClass, "_ev" -> v.toString)
    }

    case args if super.toMongoMapper.isDefined && super.toMongoMapper.get.isDefinedAt(args) => {
      super.toMongoMapper.get.apply(args)
    }
  }

  override def fromMongoMapper: OpmFromMongoMapper = Some {
    case (_, _, o: BasicDBObject) if o.contains("_ec") && o.contains("_ev") => {
      val enumClass = Class.forName(o("_ec").toString)
      val enumObject = enumClass.getField("MODULE$").get(null)
      val value = o("_ev").toString
      enumClass.getMethod("withName",classOf[String]).invoke(enumObject, value)
    }

    case args if super.fromMongoMapper.isDefined && super.fromMongoMapper.get.isDefinedAt(args) => {
      super.fromMongoMapper.get.apply(args)
    }
  }
}