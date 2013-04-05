package com.gilt.opm

import collection.mutable
import java.lang.reflect.Method
import com.gilt.opm.OpmIntrospection._
import com.gilt.opm.OpmFactory.OpmField

private [opm] case class OpmProxy(key: String, fields: Map[String, OpmField], history: Stream[OpmProxy] = Nil.toStream) {

  import OpmIntrospection._

  override def toString: String = {
    val b = new mutable.StringBuilder()
    b.append(clazz.getName).append("(")
    b.append("opmKey=%s,".format(key))
    updatedBy.foreach(u => b.append("opmUpdatedBy=%s,".format(u)))
    updateReason.foreach(u => b.append("updateReason=%s,".format(u)))
    b.append(fields.filter(f => !InternalFields.contains(f._1)).map(p => p._1 + "=" + p._2.pending.map(p => "pending until (%s)".format(p)).getOrElse(p._2.value)) mkString (","))
    b.append(")")
    b.toString()
  }

  override lazy val hashCode = {
    fields.filter(_._1 != TimestampField).hashCode()
  }

  def clazz: Class[_] = fields(ClassField).value.asInstanceOf[Class[_]]

  def timestamp: Long = fields(TimestampField).value.asInstanceOf[Long]

  // The two updater properties are Option by necessity, since end users are not required to set these values.
  def updatedBy: Option[Any] = fields.get(UpdatedByField).flatMap(_.value.asInstanceOf[Option[Any]])

  def updateReason: Option[String] = fields.get(UpdateReasonField).flatMap(_.value.asInstanceOf[Option[String]])

  def fieldMethod(field: String): Method = OpmProxy.fieldMethod(field, clazz)

  def fieldType(field: String): Class[_] = OpmProxy.fieldType(field, clazz)

  lazy val manifest: Manifest[OpmObject] = Manifest.classType(clazz)
}

object OpmProxy {
  def fieldMethod(field: String, clazz: Class[_]): Method = clazz.getMethod(field)

  /**
   * Centralizing the logic behind producing the type for a given field, so other places don't need to implement
   * the same thing.
   */
  def fieldType(field: String, clazz: Class[_]): Class[_] = field match {
    /**
     * This is a bit of a hack, but I think it makes some sense. OpmProxy doesn't know the type of updatedBy without
     * using a type parameter - which would need to be sprinkled throughout the code and seems a bit much for a
     * potentially optional type parameter. I'm not sure subclassing OpmProxy would work either. So this is relying
     * on the object to know the type of opmUpdatedBy, which it does.
     */
    case UpdatedByField => fieldMethod("opmUpdatedBy", clazz).getReturnType
    case UpdateReasonField => classOf[Option[String]]
    case _ => fieldMethod(field, clazz).getReturnType
  }
}
