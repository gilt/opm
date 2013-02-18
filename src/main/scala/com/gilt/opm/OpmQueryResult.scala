package com.gilt.opm

import query._

/**
 * This class exposes the results of an OPM query. It can be chained to further refine the query.
 *
 * Because OPM is constructed on the fly and can vary based the time you are looking for, it is difficult to build
 * a query directly on top of the underlying datastore. I guess this can vary based on implementation (depending on the
 * storage of diffs and whether or not those diffs include all properties of the object), but in the current
 * implementation there is no guarantee that two properties will be in the same record. As such, there is no way to
 * query something like [foo = 'bar' and baz = 'bat'] on a series of value and diff records, so we can effectively
 * only query by one property at a time. This class does that with Streams and filters, so the results are lazy-loaded
 * as you ask for them.
 *
 * Because it is difficult to "know" the value of an object's property until the OPM object is loaded from the
 * datastore, the methodology here is to query the datastore in the most limiting way possible (i.e. pull all records
 * for which the property matched the value at any point in time and not necessarily "right now"), then load those
 * objects and filter out any objects that do not currently match.
 *
 * For the best efficiency, it is recommended to 'find' by your most limiting query first, so as to limit the results
 * being processed. Also, it is not recommended to call obj.all.length (or a sort), since that forces the stream to be
 * completely processed and negates any advantages of the lazy-loaded stream.
 *
 * @author: Ryan Martin
 * @since: 11/1/12 6:37 PM
 */
class OpmQueryResult[T <: OpmObject : Manifest](val all: Stream[T]) {
  private val clazz = manifest[T].erasure

  /**
   * Limit the current results to n records. This can be chained for further filtering.
   *
   * @param n: The maximum number of records to include.
   * @return
   */
  def limit(n: Int): OpmQueryResult[T] = OpmQueryResult(all.take(n))

  /**
   * Use this to search within the current resultset to further filter the query. Complex queries can be chained
   * together like this:
   * obj.search(_.property1).equals("foo").search(_.property2).equals("bar")...
   *
   * @param v: A "method" that determines the property to search against.
   * @tparam V: The class of the property to search against. This helps enforce the type that is passed into the second
   *          part of the chained search call (.equals in the example above).
   * @return: Another result object, which can be further chained.
   */
  def search[V](v: T => V): OpmSearcherHelper[T, V] = {
    OpmSearcher[T](query => search(query)).search(v)
  }

  /**
   * Use this to search within the current resultset if you already have the query object. This is useful if you need
   * to collect the query, do something with it, and then pass it into an existing resultset (i.e. in
   * OpmMongoStorage.search).
   *
   * @param query: The query object to filter against.
   * @return: Another result object, which can be further chained.
   */
  def search(query: OpmPropertyQuery) = OpmQueryResult[T](all filter byQuery(query))

  private def byQuery(query: OpmPropertyQuery)(item: T): Boolean = {
    val method = clazz.getMethod(query.property)
    val currentValue = try {
      method.invoke(item)
    } catch {
      // Look for the specific case of the Mongo document missing the property (which may happen due to "schema" changes)
      case e: Exception => {
        e.getCause match {
          case ex: NoSuchElementException => if (ex.getMessage.toLowerCase.contains("key not found: %s".format(query.property.toLowerCase))) null else throw e
        }
      }
    }
    query.isMatch(currentValue)
  }
}

object OpmQueryResult {
  def apply[T <: OpmObject : Manifest](stream: Stream[T]) = new OpmQueryResult[T](stream)
}
