package com.gilt.opm

import collection.mutable

import annotation.tailrec
import java.util.concurrent.TimeUnit
import com.gilt.opm.OpmFactory.OpmField

case class Diff(field: String, newValue: Option[OpmField])

/**
 * This class is used to make sure that you are passing the right type
 * to "to". Before this, you could "set(_.foo).to(bar), and get a runtime
 * failure if bar was not the right type.
 *
 * A note to people who expand functionality here: dealing with new values happens both here (in RichOpmObject) and in
 * OpmFactory.diffModels (for evolving an existing model with an overlaid model). Make sure you make updates to both.
 */
case class RichOpmObjectSetter[T <: OpmObject, V](roo: RichOpmObject[T]) {
  def to(v: V): T = {
    roo.to(v)
  }

  /**
   * Flag a field as pending, supported in the blackboard model. This will do nothing if the field is already populated
   * with a value other than None. None will be overwritten because it is implicit acknowledgement from the developer
   * that the value can be in a "not set" state (i.e. None). Pending should overwrite this because it will potentially
   * be setting it to a Some.

   * @param expireAfter A Long representing the amount of time from now() that the pending flag should expire; use the
   *                    'unit' parameter to indicate the units of time that it represents. If the field has not been set
   *                    within this time period, the pending flag will be ignored.
   * @param unit A TimeUnit that indicates what units the expireAfter parameter is in.
   *
   */
  def toPending(expireAfter: Long, unit: TimeUnit): T = {
    roo.toPending(expireAfter, unit)
  }

  /**
   * In some manner, the pending flag can be considered a 'lock' on initiating processing for a given field in the
   * blackboard model. The use case is this: an object is being built up through a sequence of asynchronous calls to
   * external processes; setting a field to 'pending' indicates that that process has been kicked off and doesn't need
   * to be kicked off again. If that process responds with an incomplete state (e.g. a server timeout), it is useful
   * to be turn off the pending flag to allow for the processing to be initiated again (thereby 'unlocking' the field).
   * The ability to turn off the pending 'lock' is necessary in a case such as this, where there is no value to set on
   * the field and the blackboard is still building the model (i.e. a 'not set' state is entirely valid).
   */
  def toNotPending(): T = {
    roo.toNotPending()
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
      assert(stack != null)
      RichOpmObjectSetter(this)
    } finally {
      introspectionScratch.set(null)
    }
  }

  /**
   * Use this method to check if a field is pending, without resorting to a try...catch
   */
  def isPending[V](v: T => V): Boolean = {
    @tailrec
    def examine(scratch: Scratch): Boolean = {
      val pending = scratch.model.fields.get(scratch.field).flatMap(f => f.pending.map(_ > NanoTimestamp.now)).getOrElse(false)
      if (pending || stack.isEmpty) {
        pending
      } else {
        examine(stack.pop())
      }
    }

    try {
      introspect(v(obj))
      stack = introspectionScratch.get
      assert(stack != null)
      examine(stack.pop())
    } finally {
      introspectionScratch.set(null)
    }
  }

  private [opm] def to[V](v: V): T = {
    // things fall down around options ... sometimes a Some ends up here,
    // and sometimes the thing it wraps, depending on the context. So if the value
    // is not a Some, and the thing it is being stored to is an Option, then wrap it here.
    def wrap(scratch: Scratch, value: Any): OpmField = {
      // we may need to wrap this value in Some if the method return
      // type says to. Unfortunately containers are a pain with type erasure
      if (value != None && !value.asInstanceOf[AnyRef].getClass.isAssignableFrom(classOf[Some[_]]) &&
        scratch.model.fieldMethod(scratch.field).getReturnType.isAssignableFrom(classOf[Option[_]])) {
        OpmField(Option(value))
      } else {
        OpmField(value)
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

  /**
   * @see com.gilt.opm.RichOpmObjectSetter.toPending
   */
  private [opm] def toPending(expireAfter: Long, unit: TimeUnit): T = {
    @tailrec
    def populate(scratch: Scratch, value: Any, expireAt: Option[NanoTimestamp]): AnyRef = {
      if (scratch.model.fields.get(scratch.field).map(f => f.pending.forall(_ > NanoTimestamp.now) && (f.value != None)).getOrElse(false)) newProxy(scratch.model) // No change if already set to non-None
      else {
        val newFields = scratch.model.fields + (scratch.field -> OpmField(null, expireAt))
        val newModel = scratch.model.copy(fields = newFields, history = scratch.model #:: scratch.model.history)
        val newInstance = factory.instance(newModel)(new Manifest[OpmObject] {
          override val erasure = scratch.clazz
        })
        if (stack.isEmpty) {
          newInstance
        } else {
          populate(stack.pop(), newInstance, None)
        }
      }
    }
    populate(stack.pop(), null, Some(NanoTimestamp.now + TimeUnit.NANOSECONDS.convert(expireAfter, unit))).asInstanceOf[T]
  }

  /**
   * @see com.gilt.opm.RichOpmObjectSetter.toNotPending
   */
  private [opm] def toNotPending(): T = {
    @tailrec
    def populate(scratch: Scratch, value: Any): AnyRef = {
      if (scratch.model.fields.get(scratch.field).map(!_.pending.isDefined).getOrElse(false)) newProxy(scratch.model) // No change if not pending
      else {
        val newFields = scratch.model.fields + (scratch.field -> OpmField(null, None))
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
    }
    populate(stack.pop(), null).asInstanceOf[T]
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
  def diff(that: RichOpmObject[_]): Set[Diff] = {
    diffModels(model, that.model, this.obj.opmIsBuilder)
  }

  def -:-(that: RichOpmObject[T]): Set[Diff] = this.diff(that)

  def evolve(changes: Set[Diff]): T = {
    if (changes.size == 0) newProxy(model)
    else instance(model.copy(fields = OpmFactory.evolve(model.fields, changes),
      history = model #:: model.history))
  }

  def evolve(that: RichOpmObject[_]): T = evolve(that.diff(this))
}

