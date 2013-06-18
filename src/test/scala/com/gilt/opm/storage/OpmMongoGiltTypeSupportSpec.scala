package com.gilt.opm.storage

import com.gilt.opm.OpmFactory._
import com.gilt.opm.{OpmMongoStorage, CollectionHelper, OpmObject}
import com.giltgroupe.util.{CompactGuid, Timestamp}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

object OpmMongoGiltTypeSupportSpec {
  trait GiltTypes extends OpmObject {
    def compactGuid: CompactGuid[GiltTypes]
    def seqCompactGuid: Seq[CompactGuid[GiltTypes]]
    def timestamp: Timestamp
    def optTimestamp: Option[Timestamp]
  }
}
import OpmMongoGiltTypeSupportSpec._

class OpmMongoGiltTypeSupportSpec
extends FlatSpec with ShouldMatchers with OpmMongoStorage[GiltTypes] with OpmMongoGiltTypeSupport with CollectionHelper {
  val collectionName = "gilt_types"

  "OpmMongoGiltTypeSupport" should "allow extra gilt types to be stored and loaded" in {
    val gt = instance[GiltTypes]("key")
      .set(_.compactGuid).to(CompactGuid.randomCompactGuid[GiltTypes])
      .set(_.seqCompactGuid).to(Seq(CompactGuid.randomCompactGuid[GiltTypes]))
      .set(_.timestamp).to(new Timestamp())
      .set(_.optTimestamp).to(Some(new Timestamp()))
    put(gt)
    val loaded = get("key")
    assert(loaded.isDefined)
    assert(gt === loaded.get)
  }
}
