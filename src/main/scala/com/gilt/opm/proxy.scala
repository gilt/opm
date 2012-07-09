package com.gilt.opm

import java.lang.reflect.{Proxy, Method, InvocationHandler}
import collection.mutable
import annotation.tailrec

object InstanceFactory {

  case class MapProxy(fields: Map[String, Any], history: List[MapProxy] = Nil) {
    override def toString: String = {
      val b = new mutable.StringBuilder()
      val clazz = fields(ClassField).asInstanceOf[Class[_]]
      b.append(clazz.getName).append("(")
      b.append(fields.filter(_._1 != ClassField).map(p => p._1 + "=" + p._2)mkString(","))
      b.append(")")
      b.toString()
    }
  }

  private[this] val specialMethods = new Object().getClass.getMethods.collect {
    case method: Method if method.getParameterTypes.isEmpty => method.getName
  }.toSet

  private[this] val introspectionMode = {
    val t = new ThreadLocal[Boolean]
    t.set(false)
    t
  }

  private [opm] case class Scratch(model: MapProxy, field: String, clazz: Class[_])
  private [this] val introspectionScratch = new ThreadLocal[mutable.Stack[Scratch]]

  private def introspect[T](f: => T) {
    introspectionScratch.set(new mutable.Stack[Scratch])
    val was = introspectionMode.get
    introspectionMode.set(true)
    try {
      f
    } finally {
      introspectionMode.set(was)
    }
  }

  private val ClassField = "$$class$$"

  case class WeirdFieldInterceptException(method: Method) extends RuntimeException

  def instance[T <: AnyRef](implicit m: Manifest[T]): T = {
    instance(model = MapProxy(Map(ClassField -> m.erasure)))
  }

  def instance[T <: AnyRef](model: MapProxy)(implicit m: Manifest[T]) = {
    val clazz = m.erasure
    require(clazz.getName == model.fields(ClassField).asInstanceOf[Class[_]].getName)
    require(clazz.isInterface, "Only interface types cannot be created; %s is not an interface".format(clazz))
    val proxy = Proxy.newProxyInstance(clazz.getClassLoader, clazz +: clazz.getInterfaces, new InvocationHandler() {
      def invoke(proxy: Object, method: Method, args: scala.Array[Object]): AnyRef = {
        method.getName match {
          case special if specialMethods.contains(special) => method.invoke(model) // todo: really?
          case fieldName if method.getParameterTypes.isEmpty =>
            if (introspectionMode.get) {
              val stack = introspectionScratch.get()
              stack.push(Scratch(model, method.getName, clazz))
              model.fields.get(fieldName).map(_.asInstanceOf[AnyRef]).orNull
            } else {
              model.fields(fieldName).asInstanceOf[AnyRef]
            }
          case _ => throw WeirdFieldInterceptException(method)
        }
      }
    })
    proxy.asInstanceOf[T]
  }

  case class Setter[T <: AnyRef](obj: T)(implicit m: Manifest[T]) {
    private[this] var stack: mutable.Stack[Scratch] = _

    def set[V](v: T => V): Setter[T] = {
      try {
        introspect(v(obj))
        stack = introspectionScratch.get
        require(stack != null)
        this
      } catch {
        case WeirdFieldInterceptException(method) =>
          sys.error("Unexpected set field: %s".format(method.getName))
      } finally {
        introspectionScratch.set(null)
      }
    }

    def to[V](v: V): T = {
      @tailrec
      def populate(scratch: Scratch, value: Any): AnyRef = {
        val newFields = scratch.model.fields + (scratch.field -> value)
        val newModel = scratch.model.copy(fields = newFields, history = scratch.model :: scratch.model.history)
        val newInstance = instance(newModel)(new Manifest[AnyRef] {
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
  }

  implicit def toSetter[T <: AnyRef](obj: T)(implicit m: Manifest[T]): Setter[T] = Setter(obj)
}
