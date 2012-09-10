package com.gilt.opm

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/4/12 7:07 PM
 */
trait OpmStorage[V <: OpmObject] {
  def get(key: String)(implicit mf: Manifest[V]): Option[V]
  def put(obj: V)(implicit mf: Manifest[V])
  def remove(key: String)
}
