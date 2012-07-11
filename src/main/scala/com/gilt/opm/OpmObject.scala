package com.gilt.opm

trait OpmObject {
  def magic() {
    sys.error("magic should not be called")
  }

  def timestamp: Long = 0L
}

