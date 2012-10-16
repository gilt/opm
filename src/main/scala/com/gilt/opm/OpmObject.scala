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
}

