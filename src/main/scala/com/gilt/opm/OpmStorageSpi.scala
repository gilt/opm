package com.gilt.opm

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 8/22/12 12:19 PM
 */
trait OpmStorageSpi {

  def store[K, V <: OpmObject : Manifest](key: K, obj: Stream[OpmObject])
  def load[K, V <: OpmObject : Manifest](key: K): OpmObject
}
