package com.gilt.opm

import java.lang.reflect.{Proxy, Method, InvocationHandler}
import scala.collection.mutable
import com.giltgroupe.util.time.MonotonicClock
import com.gilt.opm.OpmFactory.OpmField

object OpmIntrospection {
  val ClassField = "__c__"

  val TimestampField = "__t__"

  val UpdatedByField = "__u__"

  val UpdateReasonField = "__r__"

  val MetaFields = Set(ClassField, TimestampField)

  val InternalFields = MetaFields ++ mutable.Set(UpdatedByField, UpdateReasonField)

  val UndefinedKey = "__undefined"
}

trait OpmFactory {

  def clock(): Long

  import OpmIntrospection._
  import OpmFactory.{introspectionMode, ModelExposeException, PendingOpmValueException, introspectionScratch, Scratch, recoverModel}

  /**
   * If you want to create an instance that gets persisted, you MUST give it a key when you create it.
   * Not creating a key is convenient for memory-only use cases, like the builder/blackboard features of opm.
   * @param key persistence key for the timeline. Should be unique across all timelines
   * @tparam T Type of object this should wrap
   * @return A fresh, "empty" instance
   * @see OpmObject.opmIsComplete
   */
  def instance[T <: OpmObject : Manifest](key: String = UndefinedKey): T = {
    newProxy(model = OpmProxy(key, Map(ClassField -> OpmField(manifest.runtimeClass), TimestampField -> OpmField(clock()))))
  }

  def instance[T <: OpmObject : Manifest](key: String, init: Map[String, Any]): T = {

    // new in 0.0.10: anything that is non-optional, MUST be set.
    // no nulls allowed.
    val missing = missingFields(init, ignoreOption = true)
    require(missing.isEmpty, "Not all required field set: %s".format(missing.mkString(",")))
    newProxy(model = OpmProxy(key, init.map(kv => kv._1 -> OpmField(kv._2)) ++ Map(ClassField -> OpmField(manifest.runtimeClass), TimestampField -> OpmField(clock()))))
  }

  private[opm] def instance[T <: OpmObject : Manifest](model: OpmProxy): T = {
    newProxy(model.copy(fields = model.fields + (TimestampField -> OpmField(clock()))))
  }

  // precompute field names of this object
  private [this] lazy val opmFieldNames = classOf[OpmObject].getMethods.map(_.getName).toSet

  // get hold of methods of this object
  private [opm] def fields[T <: OpmObject : Manifest]: Array[Method] =
    manifest.runtimeClass.getMethods.filterNot(m => opmFieldNames.contains(m.getName) || (m.getParameterTypes.length > 0))

  // return an array of fields not set in this map
  private [opm] def missingFields[T <: OpmObject : Manifest](init: Map[String, Any], ignoreOption: Boolean): Array[String] = {
    fields.filterNot(ignoreOption && _.getReturnType.isAssignableFrom((classOf[Option[_]]))).map(_.getName).filterNot(init.contains)
  }

  private[opm] def newProxy[T: Manifest](model: OpmProxy): T = {
    val clazz = manifest.runtimeClass
    require(clazz.getName == model.clazz.getName, "Class changed from %s to %s".format(model.clazz, clazz))
    require(clazz.isInterface, "Only interface types cannot be created; %s is not an interface".format(clazz))
    val proxy = Proxy.newProxyInstance(clazz.getClassLoader, clazz +: clazz.getInterfaces, new InvocationHandler() {
      def invoke(proxy: Object, method: Method, args: scala.Array[Object]): AnyRef = {
        method.getName match {
          case "opmMagic" =>
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
          case "opmTimestamp" =>
            if (introspectionMode.get) {
              sys.error("set(_.opmTimestamp).to(...) is not allowed (timestamps are immutable)")
            } else {
              model.timestamp.asInstanceOf[AnyRef]
            }
          case "opmKey" =>
            if (introspectionMode.get) {
              sys.error("set(_.opmKey).to(...) is not allowed (opmKey is a property of the timeline, not an instance)")
            } else {
              model.key.asInstanceOf[AnyRef]
            }
          case "opmIsComplete" =>
            java.lang.Boolean.valueOf(missingFields(model.fields, ignoreOption = false)(model.manifest).isEmpty)
          case "opmIsBuilder" =>
            (model.key == UndefinedKey).asInstanceOf[AnyRef]
          case "opmUpdatedBy" =>
            if (introspectionMode.get) {
              sys.error("set(_.opmUpdatedBy).to(...) is not allowed (opmUpdatedBy cannot be set directly)")
            } else {
              model.updatedBy.asInstanceOf[AnyRef]
            }
          case "opmUpdateReason" =>
            if (introspectionMode.get) {
              sys.error("set(_.opmUpdateReason).to(...) is not allowed (opmUpdateReason cannot be set directly)")
            } else {
              model.updateReason.asInstanceOf[AnyRef]
            }
          case fieldName if method.getParameterTypes.isEmpty =>
            if (introspectionMode.get) {
              val stack = introspectionScratch.get()
              stack.push(Scratch(model, method.getName, clazz))
              val resultOpt = model.fields.get(fieldName).map(_.value.asInstanceOf[AnyRef])
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
                  case "boolean" => false
                  case _ => null
                }).asInstanceOf[AnyRef]
              }
            } else {
              model.fields(fieldName) match {
                case OpmField(_, Some(pending)) if (pending > NanoTimestamp.now) => throw PendingOpmValueException("field '%s' on object is pending until %s".format(fieldName, pending))
                case OpmField(value, Some(pending)) => throw new NoSuchElementException("key not found: %s".format(fieldName))
                case OpmField(value, None) => value.asInstanceOf[AnyRef]
                case other => other.asInstanceOf[AnyRef]
              }
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

  def clock() = MonotonicClock.currentTimeNanos

  private [opm] case class OpmField(value: Any, pending: Option[NanoTimestamp] = None)

  private [opm] case class Scratch(model: OpmProxy, field: String, clazz: Class[_])

  private [opm] case class ModelExposeException(model: OpmProxy) extends RuntimeException

  private [opm] case class PendingOpmValueException(message: String) extends RuntimeException(message)

  private[opm] val introspectionMode = new ThreadLocal[Boolean] {
    override def initialValue() = false
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
      introspect(obj.opmMagic())
      sys.error("never executes")
    } catch {
      case ModelExposeException(model) => model
    }
  }

  private [opm] def diffModels(thisModel: OpmProxy, thatModel: OpmProxy, isBuilder: Boolean = false): Set[Diff] = {
    import OpmIntrospection._
    require(thisModel.clazz.isAssignableFrom(thatModel.clazz),
      ("We only support changing class to traits mixed into the class ( ... request additional leeway if you need it) " +
        "(this = %s, that = %s)").format(thisModel.clazz, thatModel.clazz))
    val allFields = (thisModel.fields.map(_._1).toSet ++ thatModel.fields.map(_._1)).filterNot(MetaFields)
    val diffs = for (field <- allFields if (!isBuilder || thisModel.fields.contains(field))) yield {
      (thisModel.fields.get(field), thatModel.fields.get(field)) match {
        case (None, Some(any)) => Some(Diff(field, None))
        case (any@Some(_), None) => Some(Diff(field, any))
        // If pending flag has expired, set to whatever is given
        case (any, Some(OpmField(oldVal, Some(oldPending)))) if (oldPending < NanoTimestamp.now) => Some(Diff(field, any))

        // If the value was None and is now Some or pending, set to whatever is given
        case (any@Some(OpmField(Some(_), _)), Some(OpmField(None, _))) => Some(Diff(field, any))
        case (any@Some(OpmField(_, Some(_))), Some(OpmField(None, _))) => Some(Diff(field, any))

        // If pending has been turned off and the value is missing, leave the value if already set
        case (Some(OpmField(null, None)), Some(OpmField(any, None))) => None
        // If pending has been turned off and the value is missing, simply remove the value for pending
        case (Some(OpmField(null, None)), Some(any)) => Some(Diff(field, None))
        // If the new value is pending, do not overwrite if something is already set (pending or not)
        case (Some(OpmField(newVal, Some(newPending))), Some(_)) => None
        // If the two values are different, set the new value; pending will not fall through to here
        case (value@Some(one), Some(another)) if one != another => Some(Diff(field, value))
        // If the two values are the same, do nothing
        case (Some(_), Some(_)) => None
        case (None, None) => sys.error("It should not be possible for missing values in both objects")
      }
    }
    diffs.flatten.toSet
  }

  private [opm] def evolve(fields: Map[String, OpmField], changes: Set[Diff]): Map[String, OpmField] = {

    // a change can be a modification, addition, or removal
    val changesByField: Map[String, Option[OpmField]] = changes.map(diff => diff.field -> diff.newValue).toMap

    // fields which were either modified or added, we don't care which
    val newOrChangedFields: Map[String, OpmField] = changesByField.flatMap {
      field =>
        changesByField(field._1) match {
          case None => None
          case Some(value) => Some(field._1 -> value)
        }
    }

    // fields which were added, modified, or deleted
    val touchedFields: Set[String] = changesByField.keySet

    // fields which were not modified or deleted
    val untouched: Map[String, OpmField] = fields.filterNot(f => touchedFields.contains(f._1))

    // new or modified fields ++ unmodified fields (so deleted fields get removed here)
    newOrChangedFields ++ untouched
  }
}
