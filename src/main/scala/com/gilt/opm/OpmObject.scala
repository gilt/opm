package com.gilt.opm

trait OpmObject {
  final def opmMagic() {
    sys.error("It's not allowed to implement your OpmObject trait directly. Please use OpmFactory.instance")
  }

  // we tried hard to make the type of the opmKey generic, but it caused problems with the conversion
  // to RichOpmObject, because the compiler could not reliably infer the opmKey type, and as a result
  // refuse to apply the implicit conversion.
  final def opmKey: String = ""
  final def opmTimestamp: Long = 0L

  /**
   * There are 2 ways to create an opm object: you can create an "empty" one, or you can pass in a map of initial
   * values.  If you pass in the map, opm verifies that everything that isn't optional is defined,
   * but you can also create an "empty" opm object, which will blow exceptions on access if you are trying to
   * get hold of something that doesn't exist. This method tells you whether the object is safe to access, and
   * is useful when using opm as a builder or blackboard object, to tell when it has been fully populated.
   * @return true if the object is fully populated.
   */
  final def opmIsComplete: Boolean = false

  final def opmIsBuilder: Boolean = false
}

object OpmObject {
  implicit def toSetter[T <: OpmObject : Manifest](obj: T): RichOpmObject[T] = RichOpmObject(obj)
}

/**
 * Extend this trait when you want to include auditing of updatedBy and updateReason.
 */
trait OpmAuditedObject[U] extends OpmObject {
  final def opmUpdatedBy: Option[U] = None

  final def opmUpdateReason: Option[String] = None
}

object OpmAuditedObject {
  implicit def toSetter[T <: OpmAuditedObject[U] : Manifest, U](obj: T with OpmAuditedObject[U]): RichOpmAuditObject[T, U] = RichOpmAuditObject(obj)
}