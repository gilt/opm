package com.gilt.opm

// very general stuff

import java.lang.{Long => JLong}

trait HasId {
  def id: Long
}

case class Event[T: Manifest](timestamp: Long, command: String) {
  def toChange: (String, AnyRef) = ("start", new java.util.Date(System.currentTimeMillis().asInstanceOf[JLong]))
}

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

  def append(event: Event[T]): EventLog[T] = {
    copy(event +: events)
    this
  }

  def generate(implicit m: Manifest[T]): T = {
    events.foldLeft(manifest.erasure.newInstance.asInstanceOf[T]) {
      case (instance: T, event: Event[T]) =>
        instance.mutate(event.toChange)
    }
  }
}


// start of domain specific stuff

import java.util

case class Curation(id: Long, lookElements: Seq[String]) extends Mutatable[Curation] with HasId {
  def this() = this(0l, Nil)
}

case class Sale(id: Long, curation: Seq[Curation], start: util.Date, end: Option[util.Date]) extends Mutatable[Sale] with HasId {
  def this() = this(0l, Nil, new util.Date(0), Some(new util.Date(0)))
}

object Driver extends App {
  val eventLog = EventLog[Sale]().append(
    Event[Sale](System.currentTimeMillis, "set date 1")).append(
    Event[Sale](System.currentTimeMillis, "set date 1"))
  println(eventLog.generate)
}

