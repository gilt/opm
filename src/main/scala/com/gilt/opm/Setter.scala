package com.gilt.opm

import scala.collection.mutable

import OpmHelpers.{Scratch, introspect, introspectionScratch}
import annotation.tailrec

case class Setter[T <: OpmObject](obj: T, factory: InstanceFactory)(implicit m: Manifest[T]) {
  private[this] var stack: mutable.Stack[Scratch] = _

  def set[V](v: T => V): Setter[T] = {
    try {
      introspect(v(obj))
      stack = introspectionScratch.get
      require(stack != null)
      this
    } finally {
      introspectionScratch.set(null)
    }
  }

  def to[V](v: V): T = {
    @tailrec
    def populate(scratch: Scratch, value: Any): AnyRef = {
      require(scratch.model.future.isEmpty, "Cannot set new values on objects from the past: %s".format(obj))
      val newFields = scratch.model.fields + (scratch.field -> value)
      val newModel = scratch.model.copy(fields = newFields, history = scratch.model :: scratch.model.history)
      val newInstance = factory.instance(newModel)(new Manifest[OpmObject] {
        override val erasure = scratch.clazz
      })
      if (stack.isEmpty) {
        newInstance
      } else {
        populate(stack.pop(), newInstance)
      }
    }
    populate(stack.pop(), v).asInstanceOf[T]
  }

  def :=[V](v: V): T = this.to(v)

  def timeMachine(timestamp: Long): Option[T] = {
    InstanceFactory.timeMachine(obj, timestamp)
  }
}
