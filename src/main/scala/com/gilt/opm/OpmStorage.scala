package com.gilt.opm

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/4/12 7:07 PM
 */
trait OpmStorage[K, V <: OpmObject] {
  def create(key: K, obj: V)(implicit mf: Manifest[V])
  def retrieve(key: K)(implicit mf: Manifest[V]): Option[V]
  def update(key: K, obj: OpmObject)(implicit mf: Manifest[V])
  def delete(key: K)
}
