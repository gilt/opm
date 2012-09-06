package com.gilt.opm

trait OpmObject {
  def magic() {
    sys.error("It's not allowed to implement your OpmObject trait directly. Please use OpmFactory.instance")
  }

  // we tried hard to make the type of the key generic, but it caused problems with the conversion
  // to RichOpmObject, because the compiler could not reliably infer the key type, and as a result
  // refuse to apply the implicit conversion.
  def key: String = ""
  def timestamp: Long = 0L
}

