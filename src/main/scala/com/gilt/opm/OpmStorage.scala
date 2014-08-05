package com.gilt.opm

import query.OpmSearcherHelper

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 9/4/12 7:07 PM
 */
trait OpmStorage[V <: OpmObject] {
  // This can be used to pull all records, then manipulate the results with limit and sort.
  def allRecords(implicit mf: Manifest[V]): OpmQueryResult[V]
  def get(key: String)(implicit mf: Manifest[V]): Option[V]
  def search[T](v: V => T)(implicit mf: Manifest[V]): OpmSearcherHelper[V, T]
  def put(obj: V)(implicit mf: Manifest[V])
  def remove(key: String)

  def maybePut(obj: V)(implicit mf: Manifest[V]) {
    val existing = get(obj.opmKey)
    if (existing.isEmpty || existing.get.opmTimestamp < obj.opmTimestamp) {
      put(obj)
    }
  }

  /**
   * If you want automatic storage of nested object, override this.
   *
   * Blech. The typing here is all crazy and this is the only way I could find to make it work properly. It would be
   * ideal to pass in a correct type and get back OpmStorage specific to that type. The problem is that one could
   * potentially call this method to return OpmStorage for any number of different subclasses of OpmObject. I think
   * there's some confusion (in my head, at least) around covariance and contravariance - it's possible this needs V to
   * be covariant, but all of the other places it's used it needs to be either contravariant or invariant. Anyway, I
   * have it expecting a typed obj here purely for typing purposes; as such, it is optional and None can be passed. The
   * type of the returned OpmStorage object is really determined by the manifest, which will oftentimes even override
   * the type of the passed-in obj. If you know how to clean up this mess, please do. But it works as-is and is
   * generally hidden from the end user.
   *
   * Returns an OpmStorage object for the given type.
   *
   * @param obj: Really, this is just a placeholder that enforces some typing. This is an Option because some uses of
   *           this don't require the type to come with the object and it's suitable to pass None here. The real stuff
   *           comes from mf.
   * @param mf: This is what is used in actuality to figure out the type of returned OpmStorage object.
   * @tparam T: A subclass of OpmObject.
   */
  def nestedToStorage[T <: OpmObject](obj: Option[T])(implicit mf: Manifest[T]): Option[OpmStorage[T]] = None


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
   */
  def squashPut(obj: V)(implicit mf: Manifest[V]) {
    import OpmObject._
    val toPut = get(obj.opmKey).map(obj.forceAfter(_)).getOrElse(obj.prune)
    put(toPut)
  }
}

trait OpmAuditedStorage[T <: OpmAuditedObject[U], U] extends OpmStorage[T] {
  /**
   * A squashPut option with updater fields. This is necessary because the default squashPut will use only the updater
   * information from the final diff.
   */
  def squashPut(obj: T, updatedBy: U, updateReason: String)(implicit mf: Manifest[T]) {
    import OpmAuditedObject._
    val toPut = get(obj.opmKey).map(obj.forceAfter(_)).getOrElse(obj.prune).by(updatedBy, updateReason)
    put(toPut)
  }
}
