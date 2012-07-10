package com.gilt.opm

import collection.mutable
import OpmHelpers.{ClassField, TimestampField}

case class MapProxy(fields: Map[String, Any], history: List[MapProxy] = Nil, future: List[MapProxy] = Nil) {

  override def toString: String = {
    val b = new mutable.StringBuilder()
    b.append(clazz.getName).append("(")
    b.append(fields.filter(f => f._1 != ClassField && f._1 != TimestampField).map(p => p._1 + "=" + p._2) mkString (","))
    b.append(")")
    b.toString()
  }

  override lazy val hashCode = {
    fields.filter(_._1 != TimestampField).hashCode()
  }

  def clazz: Class[_] = fields(ClassField).asInstanceOf[Class[_]]

  def timestamp: Long = fields(TimestampField).asInstanceOf[Long]
}
