package com.gilt.opm

import collection.mutable
import java.lang.reflect.Method
import com.gilt.opm.OpmFactory.OpmField

private [opm] case class OpmProxy(key: String, fields: Map[String, OpmField], history: Stream[OpmProxy] = Nil.toStream) {

  import OpmIntrospection._

  override def toString: String = {
    val b = new mutable.StringBuilder()
    b.append(clazz.getName).append("(")
    b.append("opmKey=%s,".format(key))
    b.append(fields.filter(f => !MetaFields.contains(f._1)).map(p => p._1 + "=" + p._2.pending.map(p => "pending until (%s)".format(p)).getOrElse(p._2.value)) mkString (","))
    b.append(")")
    b.toString()
  }

  override lazy val hashCode = {
    fields.filter(_._1 != TimestampField).hashCode()
  }

  def clazz: Class[_] = fields(ClassField).value.asInstanceOf[Class[_]]

  def timestamp: Long = fields(TimestampField).value.asInstanceOf[Long]

  def fieldMethod(field: String): Method = clazz.getMethod(field)

  lazy val manifest: Manifest[OpmObject] = Manifest.classType(clazz)
}
