package com.gilt.opm

import scala.collection.mutable

import annotation.tailrec

case class RichOpmObject[T <: OpmObject](obj: T, factory: OpmFactory)(implicit m: Manifest[T]) {
  import OpmFactory._

  private var stack: mutable.Stack[Scratch] = _
  private lazy val model = recoverModel(obj)

  def set[V](v: T => V): RichOpmObject[T] = {
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

  @deprecated
  def timeMachine(timestamp: Long): Option[T] = {
    factory.timeMachine(obj, timestamp)
  }

  def prune: T = {
    factory.instance(model.copy(history = List.empty, future = List.empty))
  }

  def forceAfter(currentHead: RichOpmObject[T]): T = {
    require(currentHead.model.future.isEmpty, "Cannot force in front of a time machine view")
    instance(model.copy(history = currentHead.model :: currentHead.model.history, future = List.empty))
  }

  def ::(currentHead: RichOpmObject[T]): T = forceAfter(currentHead)

  // this object and its history
  def timeline: Stream[T] = {
     obj #:: model.history.view.map(factory.newProxy(_)).toStream
  }
}
