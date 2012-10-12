package com.gilt.opm

import collection.mutable

import annotation.tailrec

case class Diff(field: String, newValue: Option[Any])

/**
 * This class is used to make sure that you are passing the right type
 * to "to". Before this, you could "set(_.foo).to(bar), and get a runtime
 * failure if bar was not the right type.
 */
case class RichOpmObjectSetter[T <: OpmObject, V](roo: RichOpmObject[T]) {
  def to(v: V): T = {
    roo.to(v)
  }

  def pruneTo(v: V): T = roo.pruneTo(v)

  def :=(v: V): T = this.to(v)

  def ::=(v: V): T = this.pruneTo(v)
}

case class RichOpmObject[T <: OpmObject : Manifest](obj: T, factory: OpmFactory) {

  import OpmFactory._

  private var stack: mutable.Stack[Scratch] = _
  private lazy val model = recoverModel(obj)

  def set[V](v: T => V): RichOpmObjectSetter[T, V] = {
    try {
      introspect(v(obj))
      stack = introspectionScratch.get
      require(stack != null)
      RichOpmObjectSetter(this)
    } finally {
      introspectionScratch.set(null)
    }
  }

  private [opm] def to[V](v: V): T = {
    // things fall down around options ... sometimes a Some ends up here,
    // and sometimes the thing it wraps, depending on the context. So if the value
    // is not a Some, and the thing it is being stored to is an Option, then wrap it here.
    def wrap(scratch: Scratch, value: Any): Any = {
      // we may need to wrap this value in Some if the method return
      // type says to. Unfortunately containers are a pain with type erasure
      if (!value.asInstanceOf[AnyRef].getClass.isAssignableFrom(classOf[Some[_]]) &&
        scratch.model.fieldMethod(scratch.field).getReturnType.isAssignableFrom(classOf[Option[_]])) {
        Option(value)
      } else {
        value
      }
    }
    @tailrec
    def populate(scratch: Scratch, value: Any): AnyRef = {
      val newFields = scratch.model.fields + (scratch.field -> wrap(scratch, value))
      val newModel = scratch.model.copy(fields = newFields, history = scratch.model #:: scratch.model.history)
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

  private [opm] def :=[V](v: V): T = this.to(v)

  private [opm] def pruneTo[V](v: V): T = {
    new RichOpmObject[T](to(v), factory).prune
  }

  private [opm] def ::=[V](v: V): T = this.pruneTo(v)

  def prune: T = {
    factory.instance(model.copy(history = Stream.empty))
  }

  def forceAfter(currentHead: RichOpmObject[T]): T = {
    instance(model.copy(history = currentHead.model #:: currentHead.model.history))
  }

  def ::(currentHead: RichOpmObject[T]): T = forceAfter(currentHead)

  // this object and its history
  def timeline: Stream[T] = {
    obj #:: model.history.view.map(factory.newProxy(_)).toStream
  }

  // a diff b returns the set of changes such that
  // b evolve (a diff b) == a
  // (so, the set of changes to transform that into this)
  def diff(that: RichOpmObject[T]): Set[Diff] = {
    diffModels(model, that.model)
  }

  def -:-(that: RichOpmObject[T]): Set[Diff] = this.diff(that)

  def evolve(changes: Set[Diff]): T = {
    instance(model.copy(fields = OpmFactory.evolve(model.fields, changes),
      history = model #:: model.history))
  }
}

