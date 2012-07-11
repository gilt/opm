package com.gilt.opm

import java.lang.reflect.{Proxy, Method, InvocationHandler}
import OpmHelpers.{ClassField, TimestampField, introspectionMode, ModelExposeException, introspectionScratch, Scratch, introspect}

trait OpmFactory {

  def clock(): Long

  def instance[T <: OpmObject](implicit m: Manifest[T]): T = {
    newProxy(model = MapProxy(Map(ClassField -> m.erasure, TimestampField -> clock())))
  }

  def instance[T <: OpmObject](model: MapProxy)(implicit m: Manifest[T]): T = {
    newProxy(model.copy(fields = model.fields + (TimestampField -> clock())))
  }

  def timeMachine[T <: OpmObject](obj: T, timestamp: Long)(implicit m: Manifest[T]): Option[T] = {
    val model = recoverModel(obj)
    if (model.history.isEmpty && model.future.isEmpty) {
      if (model.timestamp <= timestamp) {
        Some(obj)
      } else {
        None // model did not exist yet
      }
    } else {
      if (!model.future.isEmpty && timestamp > model.timestamp) {
        // look backwards in time from the future to find the first timestamp before or equal
        // new node is head of past
        val (past, future) = model.future.reverse.partition(_.timestamp <= timestamp)
        if (past.isEmpty) {
          Some(obj)
        } else {
          val newModel = past.head.copy(history = past.tail ::: model.history, future = future.reverse)
          Some(newProxy(newModel))
        }
      } else if (!model.history.isEmpty) {
        // look backwards in time from this node to find the first timestamp before or equal
        // new hode is head of past
        if (model.timestamp <= timestamp) {
          Some(obj)
        } else {
          val (past, future) = model.history.partition(_.timestamp <= timestamp)
          if (past.isEmpty) {
            None
          } else {
            val newModel = past.head.copy(history = past.tail, future = model :: future.reverse ::: model.future)
            Some(newProxy(newModel))
          }
        }
      } else {
        // history is empty, future is not, and timestamp <= model.timestamp
        if (timestamp < model.timestamp) {
          None
        } else {
          Some(obj)
        }
      }
    }
  }

  implicit def toSetter[T <: OpmObject](obj: T)(implicit m: Manifest[T]): Setter[T] = Setter(obj, this)

  private def newProxy[T](model: MapProxy)(implicit m: Manifest[T]): T = {
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
          case "toString" => method.invoke(model)
          case "hashCode" => method.invoke(model)
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
          case "equals" =>
            if (args(0).isInstanceOf[OpmObject]) {
              val bModel = recoverModel(args(0).asInstanceOf[OpmObject])
              val a = model.fields.filter(_._1 != TimestampField)
              val b = bModel.fields.filter(_._1 != TimestampField)
              (a == b).asInstanceOf[AnyRef]
            } else {
              false.asInstanceOf[AnyRef]
            }
          case unknown => method.invoke(model)
        }
      }
    })
    proxy.asInstanceOf[T]
  }

  private def recoverModel[T <: OpmObject](obj: T): MapProxy = {
    try {
      introspect(obj.magic)
      sys.error("never executes")
    } catch {
      case ModelExposeException(model) => model
    }
  }
}

object OpmFactory extends OpmFactory {
  def clock() = System.currentTimeMillis
}
