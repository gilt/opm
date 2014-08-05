package com.gilt.opm.storage

import com.gilt.opm.OpmFactory._
import com.gilt.opm.{OpmMongoStorage, CollectionHelper, OpmObject}
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object TopLevelEnum extends Enumeration {
  val V = Value
  val V_NAMED = Value("named")
}

object OpmMongoEnumerationSupportSpec {
  object NestedEnum extends Enumeration {
    val V = Value
    val V_NAMED = Value("named")
  }

  trait WithEnums extends OpmObject {
    def topLevelEnum: TopLevelEnum.Value
    def topLevelEnum_named: TopLevelEnum.Value
    def seqTopLevelEnum: Seq[TopLevelEnum.Value]

    def nestedEnum: NestedEnum.Value
    def nestedEnum_named: NestedEnum.Value
    def optNestedEnum_named: Option[NestedEnum.Value]
  }
}
import OpmMongoEnumerationSupportSpec._

class OpmMongoEnumerationSupportSpec
extends FlatSpec with Matchers with OpmMongoStorage[WithEnums] with OpmMongoEnumerationSupport with CollectionHelper {
  val collectionName = "enumerations"

  "OpmMongoEnumerationSupport" should "allow enumerations to be stored and loaded" in {
    val we = instance[WithEnums]("key")
      .set(_.topLevelEnum).to(TopLevelEnum.V)
      .set(_.topLevelEnum).to(TopLevelEnum.V_NAMED)
      .set(_.seqTopLevelEnum).to(Seq(TopLevelEnum.V))
      .set(_.nestedEnum).to(NestedEnum.V)
      .set(_.nestedEnum).to(NestedEnum.V_NAMED)
      .set(_.optNestedEnum_named).to(Some(NestedEnum.V_NAMED))
    put(we)
    val loaded = get("key")
    assert(loaded.isDefined)
    assert(we === loaded.get)
  }
}
