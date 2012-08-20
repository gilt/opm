package com.gilt.opm

import collection.mutable

import annotation.tailrec

case class Diff(field: String, newValue: Option[Any])

case class RichOpmObject[T <: OpmObject : Manifest](obj: T, factory: OpmFactory) {

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

  def pruneTo[V](v: V): T = this.to(v).prune

  def ::=[V](v: V): T = this.pruneTo(v)

  def prune: T = {
    factory.instance(model.copy(history = List.empty))
  }

  def forceAfter(currentHead: RichOpmObject[T]): T = {
    instance(model.copy(history = currentHead.model :: currentHead.model.history))
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
    import OpmFactory.metaFields
    val thisModel = model
    val thatModel = that.model
    require(thisModel.clazz.getName == thatModel.clazz.getName,
      ("We don't support changing class (yet ... request the feature if you need it) " +
        "(this = %s, that = %s)").format(thisModel.clazz, thatModel.clazz))
    val allFields = (thisModel.fields.map(_._1).toSet ++ thatModel.fields.map(_._1)).filterNot(metaFields)
    val diffs = for (field <- allFields) yield {
      (thisModel.fields.get(field), thatModel.fields.get(field)) match {
        case (None, Some(any)) => Some(Diff(field, None))
        case (any@Some(_), None) => Some(Diff(field, any))
        case (value@Some(one), Some(another)) if one != another => Some(Diff(field, value))
        case (Some(_), Some(_)) => None
        case (None, None) => sys.error("It should not be possible for missing values in both objects")
      }
    }
    diffs.flatten.toSet
  }

  def evolve(changes: Set[Diff]): T = {
    val byField = changes.map(diff => diff.field -> diff.newValue).toMap
    val newFields = byField.flatMap {
      field =>
        byField(field._1) match {
          case None => None
          case Some(value) => Some(field._1 -> value)
        }
    }

    instance(model.copy(fields = newFields ++ model.fields.filterNot(f => byField.keySet.contains(f._1)),
      history = model :: model.history))
  }
}
