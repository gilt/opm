package com.gilt.opm

// very general stuff

import java.lang.{Long => JLong}
import collection.mutable.ListBuffer

trait HasId {
  def id: Long
}

case class Event[T: Manifest](command: Command, timestamp: Long = System.currentTimeMillis)

trait Mutatable[T] {
  def mutate(changes: (String, AnyRef)*): T = {
    val arguments: Array[AnyRef] = for {
      field <- getClass.getDeclaredFields
      fieldName = field.getName
    } yield {
      changes.find(_._1 == fieldName).map(_._2).getOrElse {
        getClass.getMethod(fieldName).invoke(this)
      }
    }
    val constructors = getClass.getDeclaredConstructors
    val constructor = constructors.find(_.getParameterTypes.size == arguments.size)
    constructor.map(_.newInstance(arguments: _*).asInstanceOf[T]).getOrElse {
      sys.error("Could not find a constructor suitable for %s from %s".format(
        arguments.mkString(","), constructors.mkString(",")))
    }
  }
}

case class EventLog[T <: Mutatable[T]](events: Seq[Event[T]] = Vector()) {

  def reify(implicit m: Manifest[T]): T = {
    val clazz = events.head.command.value.asInstanceOf[Class[T]]
    val obj = events.head.command.value.asInstanceOf[Class[T]].newInstance
    val changes = events.tail.map(_.command).map {
      case Command(SetOp, Some(field), value) => (field -> value.asInstanceOf[AnyRef])
      case cmd => sys.error("Could not reify command %s".format(cmd))
    }
    obj.mutate(changes: _*)
  }
}

object EventLog {
  def snapshot[T <: Mutatable[T]](obj: T)(implicit m: Manifest[T]): EventLog[T] = {
    val buffer = new ListBuffer[Event[T]]
    buffer += Event(Command(CreateOp, None, obj.getClass))
    buffer ++= {
      for {
        field <- m.erasure.getDeclaredFields
        method <- m.erasure.getMethods.find(_.getName == field.getName)
      } yield {
        Event(Command(SetOp, Some(field.getName), method.invoke(obj)))
      }
    }
    EventLog[T](buffer.toSeq)
  }
}




