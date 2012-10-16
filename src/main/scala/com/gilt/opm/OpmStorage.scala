package com.gilt.opm

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/4/12 7:07 PM
 */
trait OpmStorage {
  def get[V <: OpmObject](key: String)(implicit mf: Manifest[V]): Option[V]
  def put[V <: OpmObject](obj: V)(implicit mf: Manifest[V])
  def remove(key: String)

  def maybePut[V <: OpmObject](obj: V)(implicit mf: Manifest[V]) {
    val existing = get(obj.opmKey)
    if (existing.isEmpty || existing.get.opmTimestamp < obj.opmTimestamp) {
      put(obj)
    }
  }
}
