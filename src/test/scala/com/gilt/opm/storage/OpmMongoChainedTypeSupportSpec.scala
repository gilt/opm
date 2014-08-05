package com.gilt.opm.storage

import com.gilt.gfc.id.Guid
import com.gilt.gfc.time.Timestamp
import org.scalatest.FunSpec
import com.gilt.opm.{OpmMongoStorage, CollectionHelper, OpmObject}
import org.scalatest.Matchers
import com.gilt.opm.OpmFactory._

/**
 * A suite of tests to exercise OpmMongo*TypeSupport classes, chained together.
 */
object TestEnum extends Enumeration {
  val V = Value
  val V_NAMED = Value("named")
}

object OpmMongoChainedTypeSupportSpec {
  trait ChainedTypes extends OpmObject {
    def bigInt: BigInt
    def bigDecimal: BigDecimal
    def typedGuid: Guid[ChainedTypes]
    def timestamp: Timestamp
    def testEnum: TestEnum.Value
    def testEnum_named: TestEnum.Value
  }
}
import OpmMongoChainedTypeSupportSpec._

class OpmMongoChainedTypeSupportSpec
  extends FunSpec
  with Matchers
  with OpmMongoStorage[ChainedTypes]
  with OpmMongoBasicTypeSupport
  with OpmMongoEnumerationSupport
  with OpmMongoGiltTypeSupport
  with CollectionHelper
{
  val collectionName = "chained_type_support"

  describe("chaining support") {
    it("should work when multiple TypeSupport traits are mixed into the same storage class") {
      val ct = instance[ChainedTypes]("key")
        .set(_.bigInt).to(BigInt(2) << 128)
        .set(_.bigDecimal).to(BigDecimal(1) / 3)
        .set(_.testEnum).to(TestEnum.V)
        .set(_.testEnum_named).to(TestEnum.V_NAMED)
        .set(_.typedGuid).to(Guid.randomGuid[ChainedTypes]())
        .set(_.timestamp).to(new Timestamp())
      put(ct)
      val loaded = get("key")
      assert(loaded.isDefined)
      assert(ct === loaded.get)

    }
  }
}
