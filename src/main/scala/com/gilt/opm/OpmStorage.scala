package com.gilt.opm

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 8/22/12 12:18 PM
 */
object OpmStorage {

  import OpmFactory._

  def store[K, V <: OpmObject](key: K, obj: OpmObject)(implicit spi: OpmStorageSpi, m: Manifest[V]) {
    spi.store(key, obj.timeline)
  }

  def load[K, V <: OpmObject](key: K)(implicit spi: OpmStorageSpi, m: Manifest[V]): Option[V] = {
    None
  }
}
