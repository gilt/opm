package com.gilt.opm.storage

import com.gilt.opm._

/**
 * When adding in type support as mixable traits, extend from this trait to streamline some of the complexity
 * around chaining multiple TypeSupport traits.
 *
 * See [[OpmMongoBasicTypeSupport]] for the best way to implement this. Generally, you will likely define a
 * PartialFunction, followed by 'orElse super.toMongoMapper' or 'orElse super.fromMongoMapper'. Due to strong typing,
 * you will likely need to define the PartialFunction in a local val.
 */
trait MongoMapper {
  def toMongoMapper: OpmToMongoMapper
  def fromMongoMapper: OpmFromMongoMapper
}
