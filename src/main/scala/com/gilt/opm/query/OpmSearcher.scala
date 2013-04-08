package com.gilt.opm.query

import collection.mutable
import com.gilt.opm.OpmFactory._
import com.gilt.opm.OpmFactory.Scratch
import com.gilt.opm.{OpmQueryResult, OpmObject, OpmFactory}

/**
 * A class to help determine correct typing on searches for a given OpmObject T.
 *
 * The case class captures the instance properties of the search (the property that was requested in the search, the
 * callback function, etc) and spins off a call to OpmSearcherHelper. OpmSearcherHelper uses generic typing to preserve
 * the types of both the calling object and the requested property; it can then enforce correct typing when it calls
 * back to the original searcher object.
 *
 * @param finishSearch: Clients of this search must provide a callback that will be called once the search property
 *                    and matching value have been collected. The callback function takes OpmPropertyQuery and returns
 *                    an OpmQueryResult.
 *
 * @param valueTranslator: Depending on the storage format, you may need to help it translate the searched-for values.
 *                       For example, Mongo needs clean JSON, which is not necessarily parseable for non-standard
 *                       values. The storage model will have parsed values, but when searching it will need to parse
 *                       the searched-for value at search-time. If your storage model needs this support, you can
 *                       optionally pass in a translator here. By necessity, this is sprinkled throughout dependent
 *                       classes.
 *
 * @author: Ryan Martin
 * @since: 11/2/12 8:38 PM
 */
case class OpmSearcher[T <: OpmObject : Manifest](finishSearch: OpmPropertyQuery => OpmQueryResult[T], valueTranslator: Option[(String, Any) => Any] = None) {
  private var stack: mutable.Stack[Scratch] = _

  /**
   * This kicks off a search by collecting the object property to be searched against. It uses introspection to capture
   * both the name and type of the property. It kicks the search over to OpmSearcherHelper with the specific types of
   * the search (T - the object's class, and V - the property's class), so OpmSearcherHelper can in turn enforce those
   * same types when completing the search.
   *
   * @param v: A 'method' that returns the property to be searched against. In practice, this will look
   *         like: obj.search(_.propertyName). This can then be chained with a call to one of OpmSearcherHelper's
   *         methods, to the final search call looks like this: obj.search(_.properyName).equals("name")
   * @tparam V: This is inferred from the property given. It is captured and passed to OpmSearcherHelper.
   * @return
   */
  def search[V](v: T => V): OpmSearcherHelper[T, V] = {
    try {
      introspect(v(OpmFactory.instance[T]("{search}")))
      stack = introspectionScratch.get
      require(stack != null)
      OpmSearcherHelper(this)
    } finally {
      introspectionScratch.set(null)
    }
  }

  /**
   * Matches records where property is between start and end, including those values.
   *
   * @param start
   * @param end
   * @return
   */
  private [query] def between[V <% Ordered[V]](start: V, end: V) = finishSearch(OpmPropertyBetween(property, start, end, valueTranslator))

  /**
   * Matches records where property is between start and end, excluding those values.
   *
   * @param start
   * @param end
   * @return
   */
  private [query] def betweenExcl[V <% Ordered[V]](start: V, end: V) = finishSearch(OpmPropertyBetweenExclusive(property, start, end, valueTranslator))

  /**
   * Matches records where the (array) property contains the given value.
   *
   * @param value
   * @return
   */
  private [query] def contains[V](value: V) = finishSearch(OpmPropertyContains(property, value, valueTranslator))

  /**
   * Matches records where property == value
   *
   * @param value
   * @return
   */
  private [query] def eql[V](value: V) = finishSearch(OpmPropertyEquals(property, value, valueTranslator))

  /**
   * Matches records where property > value
   *
   * @param value
   * @return
   */
  private [query] def gt[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyGreaterThan(property, value, valueTranslator))

  /**
   * Matches records where property >= value
   *
   * @param value
   * @return
   */
  private [query] def gte[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyGreaterThanOrEqual(property, value, valueTranslator))

  /**
   * Matches records where property is included in the given array.
   *
   * @param values
   * @return
   */
  private [query] def in[V](values: Iterable[V]) = finishSearch(OpmPropertyIn(property, values, valueTranslator))

  /**
   * Matches records in which the property is not defined.
   *
   * Further refactoring may open this up to empty-string, but I'm leaving it as blank only for now.
   *
   * @return
   */
  private [query] def isBlank() = finishSearch(OpmPropertyBlank(property))

  /**
   * Matches records where property < value
   *
   * @param value
   * @return
   */
  private [query] def lt[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyLessThan(property, value, valueTranslator))

  /**
   * Matches records where property <= value
   *
   * @param value
   * @return
   */
  private [query] def lte[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyLessThanOrEqual(property, value, valueTranslator))

  /**
   * Collects the introspected type stack into a single string delimited by ".". The assumption is that your datastore
   * can correctly interpret this format to get to any nested objects. For example, if you're using Mongo and your
   * object looks like this: obj.foo.bar, the Mongo document should generally look like this: {'foo': {'bar': 'value'}}.
   */
  private lazy val property: String = stack.foldLeft("")((acc, item) => "%s%s%s".format(item.field, if (acc == "") "" else ".", acc))
}

case class OpmSearcherHelper[T <: OpmObject, V](result: OpmSearcher[T]) {
  def equals(v: V) = result.eql(v)
  def ===(v: V) = this.equals(v)

  def in(v: Iterable[V]) = result.in(v)

  def isBlank() = result.isBlank()
}

class OpmSearcherHelperWithIterable[T <: OpmObject, V, U <: Iterable[V]](refer: OpmSearcherHelper[T, U]) {
  def contains(v: V) = refer.result.contains(v)
}

class OrderedOpmSearcherHelper[T <: OpmObject, V <% Ordered[V]](refer: OpmSearcherHelper[T, V]) {
  def between(start: V, end: V)(implicit f : V => Ordered[V]) = refer.result.between(start, end)

  def betweenExclusive(start: V, end: V)(implicit f : V => Ordered[V]) = refer.result.betweenExcl(start, end)

  def gt(v: V)(implicit f : V => Ordered[V]) = refer.result.gt(v)
  def greaterThan(v: V)(implicit f : V => Ordered[V]) = this.gt(v)

  def gte(v: V)(implicit f : V => Ordered[V]) = refer.result.gte(v)
  def greaterThanOrEqual(v: V)(implicit f : V => Ordered[V]) = this.gte(v)

  def lt(v: V)(implicit f : V => Ordered[V]) = refer.result.lt(v)
  def lessThan(v: V)(implicit f : V => Ordered[V]) = this.lt(v)

  def lte(v: V)(implicit f : V => Ordered[V]) = refer.result.lte(v)
  def lessThanOrEqual(v: V)(implicit f : V => Ordered[V]) = this.lte(v)
}

object OpmSearcherHelper {
  implicit def nonorderedToOrdered[T <: OpmObject, V <% Ordered[V]](orig: OpmSearcherHelper[T, V]) = new OrderedOpmSearcherHelper[T, V](orig)
  implicit def nonWithIterableToWithIterable[T <: OpmObject, V](orig: OpmSearcherHelper[T, Iterable[V]]) = new OpmSearcherHelperWithIterable[T, V, Iterable[V]](orig)
  // These are a bit hack-ish, but I can't quite invoke the correct co-/contra-variance voodoo here to make these
  // implicits work without forcing it here. Feel free to fix if you can.
  implicit def nonWithIterableToWithSeq[T <: OpmObject, V](orig: OpmSearcherHelper[T, Seq[V]]) = new OpmSearcherHelperWithIterable[T, V, Seq[V]](orig)
  implicit def nonWithIterableToWithSet[T <: OpmObject, V](orig: OpmSearcherHelper[T, Set[V]]) = new OpmSearcherHelperWithIterable[T, V, Set[V]](orig)
}
