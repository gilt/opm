package com.gilt.opm

import OpmFactory.toSetter

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

  /**
   * Use this to put a batch of changes as a single diff, without losing history.
   *
   * OPM can be a bit verbose when storing diffs as individual records in the database. Use this method to take all
   * of your current unwritten changes, squash them into one single change, then push them to the top of the object's
   * history. For example, imagine your implementation is in Postgres and triggers save off your changes when you
   * update a table.
   *
   * put would look like this:
   * update my_table set foo = '1' where id = 1234;
   * update my_table set bar = '2' where id = 1234;
   * update my_table set baz = '3' where id = 1234;
   *
   * but squashPut would look like this:
   * update my_table set foo = '1', bar = '2', baz = '3' where id = 1234;
   *
   * @param obj: The current version of the object to persist.
   * @tparam V
   */
  def squashPut[V <: OpmObject: Manifest](obj: V) {
    val toPut = get(obj.opmKey).map(obj.forceAfter(_)).getOrElse(obj.prune)
    put(toPut)
  }
}
