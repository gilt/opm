package com.gilt.opm

import java.lang.reflect.{Proxy, Method, InvocationHandler}
import scala.collection.mutable

trait OpmFactory {

  def clock(): Long

  import OpmFactory.{ClassField, TimestampField, introspectionMode, ModelExposeException, introspectionScratch, Scratch, recoverModel}

  def instance[T <: OpmObject](implicit m: Manifest[T]): T = {
    newProxy(model = OpmProxy(Map(ClassField -> m.erasure, TimestampField -> clock())))
  }

  implicit def toSetter[T <: OpmObject](obj: T)(implicit m: Manifest[T]): RichOpmObject[T] = RichOpmObject(obj, this)

  private[opm] def instance[T <: OpmObject](model: OpmProxy)(implicit m: Manifest[T]): T = {
    newProxy(model.copy(fields = model.fields + (TimestampField -> clock())))
  }

  private[opm] def newProxy[T](model: OpmProxy)(implicit m: Manifest[T]): T = {
    val clazz = m.erasure
    require(clazz.getName == model.clazz.getName, "Class changed from %s to %s".format(model.clazz, clazz))
    require(clazz.isInterface, "Only interface types cannot be created; %s is not an interface".format(clazz))
    val proxy = Proxy.newProxyInstance(clazz.getClassLoader, clazz +: clazz.getInterfaces, new InvocationHandler() {
      def invoke(proxy: Object, method: Method, args: scala.Array[Object]): AnyRef = {
        method.getName match {
          case "magic" =>
            if (introspectionMode.get) {
              throw ModelExposeException(model)
            } else {
              sys.error("magic method should not be called")
            }
          case "toString" =>
            method.invoke(model)
          case "hashCode" =>
            method.invoke(model)
          case "equals" =>
            if (args(0).isInstanceOf[OpmObject]) {
              val bModel = recoverModel(args(0).asInstanceOf[OpmObject])
              val a = model.fields.filter(_._1 != TimestampField)
              val b = bModel.fields.filter(_._1 != TimestampField)
              (a == b).asInstanceOf[AnyRef]
            } else {
              false.asInstanceOf[AnyRef]
            }
          case "timestamp" =>
            model.timestamp.asInstanceOf[AnyRef]
          case fieldName if method.getParameterTypes.isEmpty =>
            if (introspectionMode.get) {
              val stack = introspectionScratch.get()
              stack.push(Scratch(model, method.getName, clazz))
              val resultOpt = model.fields.get(fieldName).map(_.asInstanceOf[AnyRef])
              if (resultOpt.isDefined) {
                resultOpt.get
              } else {
                (method.getReturnType.getName match {
                  case "byte" => 0.asInstanceOf[Byte]
                  case "char" => 0.asInstanceOf[Char]
                  case "short" => 0.asInstanceOf[Short]
                  case "int" => 0
                  case "long" => 0.asInstanceOf[Long]
                  case "float" => 0.asInstanceOf[Float]
                  case "double" => 0.asInstanceOf[Double]
                  case _ => null
                }).asInstanceOf[AnyRef]
              }
            } else {
              model.fields(fieldName).asInstanceOf[AnyRef]
            }
          case unknown =>
            sys.error("Unknown method: %s".format(unknown))
        }
      }
    })
    proxy.asInstanceOf[T]
  }
}

object OpmFactory extends OpmFactory {

  def clock() = System.currentTimeMillis

  private [opm] val ClassField = "$$class$$"

  private [opm] val TimestampField = "$$timestamp$$"

  private [opm] case class Scratch(model: OpmProxy, field: String, clazz: Class[_])

  private [opm] case class ModelExposeException(model: OpmProxy) extends RuntimeException

  private [opm] val introspectionMode = {
    val t = new ThreadLocal[Boolean]
    t.set(false)
    t
  }

  private [opm] val introspectionScratch = new ThreadLocal[mutable.Stack[Scratch]]

  private [opm] def introspect[T](f: => T): T = {
    introspectionScratch.set(new mutable.Stack[Scratch])
    val was = introspectionMode.get
    introspectionMode.set(true)
    try {
      f
    } finally {
      introspectionMode.set(was)
    }
  }

  private [opm] def recoverModel[T <: OpmObject](obj: T): OpmProxy = {
    try {
      introspect(obj.magic())
      sys.error("never executes")
    } catch {
      case ModelExposeException(model) => model
    }
  }
}
