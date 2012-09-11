package com.gilt.opm

import collection.mutable
import java.lang.reflect.Method

private [opm] object OpmProxy {
  def apply(history: Stream[OpmProxy]): OpmProxy = {
    OpmProxy(history.head.key, history.head.fields, history)
  }
}

private [opm] case class OpmProxy(key: String, fields: Map[String, Any], history: Stream[OpmProxy] = Nil.toStream) {

  import OpmIntrospection._

  override def toString: String = {
    val b = new mutable.StringBuilder()
    b.append(clazz.getName).append("(")
    b.append("key=%s,".format(key))
    b.append(fields.filter(f => f._1 != ClassField && f._1 != TimestampField).map(p => p._1 + "=" + p._2) mkString (","))
    b.append(")")
    b.toString()
  }

  override lazy val hashCode = {
    fields.filter(_._1 != TimestampField).hashCode()
  }

  def clazz: Class[_] = fields(ClassField).asInstanceOf[Class[_]]

  def timestamp: Long = fields(TimestampField).asInstanceOf[Long]

  def fieldMethod(field: String): Method = clazz.getMethod(field)
}
