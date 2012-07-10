package com.gilt.opm

import java.lang.reflect.Method
import scala.collection.mutable

// package object doesn't work here because of https://issues.scala-lang.org/browse/SI-5504
private [opm] object OpmHelpers {

  val introspectionMode = {
    val t = new ThreadLocal[Boolean]
    t.set(false)
    t
  }
  val introspectionScratch = new ThreadLocal[mutable.Stack[Scratch]]

  def introspect[T](f: => T): T = {
    introspectionScratch.set(new mutable.Stack[Scratch])
    val was = introspectionMode.get
    introspectionMode.set(true)
    try {
      f
    } finally {
      introspectionMode.set(was)
    }
  }

  case class Scratch(model: MapProxy, field: String, clazz: Class[_])

  val ClassField = "$$class$$"
  val TimestampField = "$$timestamp$$"


  case class ModelExposeException(model: MapProxy) extends RuntimeException
}
