package com.gilt.opm

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/4/12 7:07 PM
 */
trait OpmStorage[V <: OpmObject] {
  def create(obj: V)(implicit mf: Manifest[V])
  def retrieve(key: String)(implicit mf: Manifest[V]): Option[V]
  def update(obj: V)(implicit mf: Manifest[V])
  def delete(key: String)
}
