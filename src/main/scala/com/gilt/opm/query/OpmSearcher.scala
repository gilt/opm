package com.gilt.opm.query

import collection.mutable
import com.gilt.opm.OpmFactory._
import com.gilt.opm.OpmFactory.Scratch
import com.gilt.opm.{OpmQueryResult, OpmObject, OpmFactory}
import scala.language.implicitConversions

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
 * @param matchInverse: Match the inverse of the chosen query, i.e. 'not'.
 *
 * @param valueTranslator: Depending on the storage format, you may need to help it translate the searched-for values.
 *                       For example, Mongo needs clean JSON, which is not necessarily parseable for non-standard
 *                       values. The storage model will have parsed values, but when searching it will need to parse
 *                       the searched-for value at search-time. If your storage model needs this support, you can
 *                       optionally pass in a translator here. By necessity, this is sprinkled throughout dependent
 *                       classes.
 *
 * @param stackOverride: Used only by OpmSearcherHelper to immutably create a 'not' query. Generally shouldn't be used.
 */
case class OpmSearcher[T <: OpmObject : Manifest](finishSearch: (OpmPropertyQuery, Boolean) => OpmQueryResult[T],
                                                  matchInverse: Boolean = false,
                                                  valueTranslator: Option[(String, Any) => Any] = None,
                                                  stackOverride: mutable.Stack[Scratch] = null) {
  private[query] var stack: mutable.Stack[Scratch] = stackOverride

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
   */
  private [query] def between[V <% Ordered[V]](start: V, end: V) = finishSearch(OpmPropertyBetween(property, start, end, valueTranslator), matchInverse)

  /**
   * Matches records where property is between start and end, excluding those values.
   */
  private [query] def betweenExcl[V <% Ordered[V]](start: V, end: V) = finishSearch(OpmPropertyBetweenExclusive(property, start, end, valueTranslator), matchInverse)

  /**
   * Matches records where the (array) property contains the given value.
   */
  private [query] def contains[V](value: V) = finishSearch(OpmPropertyContains(property, value, valueTranslator), matchInverse)

  /**
   * Matches records where property == value
   */
  private [query] def eql[V](value: V) = finishSearch(OpmPropertyEquals(property, value, valueTranslator), matchInverse)

  /**
   * Matches records where property > value
   */
  private [query] def gt[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyGreaterThan(property, value, valueTranslator), matchInverse)

  /**
   * Matches records where property >= value
   */
  private [query] def gte[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyGreaterThanOrEqual(property, value, valueTranslator), matchInverse)

  /**
   * Matches records where property is included in the given array.
   */
  private [query] def in[V](values: Iterable[V]) = finishSearch(OpmPropertyIn(property, values, valueTranslator), matchInverse)

  /**
   * Matches records in which the property is not defined.
   *
   * Further refactoring may open this up to empty-string, but I'm leaving it as blank only for now.
   */
  private [query] def isBlank() = finishSearch(OpmPropertyBlank(property), matchInverse)

  /**
   * Matches records where property < value
   */
  private [query] def lt[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyLessThan(property, value, valueTranslator), matchInverse)

  /**
   * Matches records where property <= value
   */
  private [query] def lte[V <% Ordered[V]](value: V) = finishSearch(OpmPropertyLessThanOrEqual(property, value, valueTranslator), matchInverse)

  /**
   * Collects the introspected type stack into a single string delimited by ".". The assumption is that your datastore
   * can correctly interpret this format to get to any nested objects. For example, if you're using Mongo and your
   * object looks like this: obj.foo.bar, the Mongo document should generally look like this: {'foo': {'bar': 'value'}}.
   */
  private lazy val property: String = stack.foldLeft("")((acc, item) => "%s%s%s".format(item.field, if (acc == "") "" else ".", acc))
}

case class OpmSearcherHelper[T <: OpmObject, V](result: OpmSearcher[T])(implicit mf: Manifest[T]) {
  def equals(v: V) = result.eql(v)
  def ===(v: V) = this.equals(v)

  def in(v: Iterable[V]) = result.in(v)

  def isBlank() = result.isBlank()

  def not() = this.copy[T, V](result.copy[T](matchInverse = true, stackOverride = result.stack))
}

class OpmSearcherHelperWithIterable[T <: OpmObject, V, U <: Iterable[V]](refer: OpmSearcherHelper[T, U]) {
  def contains(v: V) = refer.result.contains(v)
}

class OrderedOpmSearcherHelper[T <: OpmObject, V <% Ordered[V]](refer: OpmSearcherHelper[T, V]) {
  def between(start: V, end: V) = refer.result.between(start, end)

  def betweenExclusive(start: V, end: V) = refer.result.betweenExcl(start, end)

  def gt(v: V) = refer.result.gt(v)
  def greaterThan(v: V) = this.gt(v)

  def gte(v: V) = refer.result.gte(v)
  def greaterThanOrEqual(v: V) = this.gte(v)

  def lt(v: V) = refer.result.lt(v)
  def lessThan(v: V) = this.lt(v)

  def lte(v: V) = refer.result.lte(v)
  def lessThanOrEqual(v: V) = this.lte(v)
}

object OpmSearcherHelper {
  implicit def nonorderedToOrdered[T <: OpmObject, V <% Ordered[V]](orig: OpmSearcherHelper[T, V]) = new OrderedOpmSearcherHelper[T, V](orig)
  implicit def nonWithIterableToWithIterable[T <: OpmObject, V](orig: OpmSearcherHelper[T, Iterable[V]]) = new OpmSearcherHelperWithIterable[T, V, Iterable[V]](orig)
  // These are a bit hack-ish, but I can't quite invoke the correct co-/contra-variance voodoo here to make these
  // implicits work without forcing it here. Feel free to fix if you can.
  implicit def nonWithIterableToWithSeq[T <: OpmObject, V](orig: OpmSearcherHelper[T, Seq[V]]) = new OpmSearcherHelperWithIterable[T, V, Seq[V]](orig)
  implicit def nonWithIterableToWithSet[T <: OpmObject, V](orig: OpmSearcherHelper[T, Set[V]]) = new OpmSearcherHelperWithIterable[T, V, Set[V]](orig)
}
