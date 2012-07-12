package com.gilt.opm

trait OpmObject {
  def magic() {
    sys.error("It's not allowed to implement your OpmObject trait directly. Please use OpmFactory.instance")
  }

  def timestamp: Long = 0L
}

