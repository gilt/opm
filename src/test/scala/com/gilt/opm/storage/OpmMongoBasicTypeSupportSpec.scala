package com.gilt.opm.storage

import com.gilt.opm.OpmFactory._
import com.gilt.opm.{OpmMongoStorage, CollectionHelper, OpmObject}
import java.net.{InetAddress, Inet4Address, Inet6Address}
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object OpmMongoBasicTypeSupportSpec {
  trait BasicTypes extends OpmObject {
    def address4: Inet4Address
    def seqAddress4s: Seq[Inet4Address]
    def address6: Inet6Address
    def setAddress6s: Set[Inet6Address]
    def bigInt: BigInt
    def listBigInts: List[BigInt]
    def bigDecimal: BigDecimal
    def optBigDecimal: Option[BigDecimal]
    def mapBigDecimalValues: Map[String, BigDecimal]
    def mapBigDecimalKeys: Map[BigDecimal, String]
  }
}
import OpmMongoBasicTypeSupportSpec._

class OpmMongoBasicTypeSupportSpec
  extends FlatSpec
  with Matchers
  with OpmMongoStorage[BasicTypes]
  with OpmMongoBasicTypeSupport
  with CollectionHelper {

  val collectionName = "basic_types"

  "OpmMongoBasicTypeSupport" should "allow extra basic types to be stored and loaded" in {
    val bt = instance[BasicTypes]("key")
      .set(_.address4).to(InetAddress.getByName("127.0.0.1").asInstanceOf[Inet4Address])
      .set(_.seqAddress4s).to(Seq(InetAddress.getByName("127.0.0.1").asInstanceOf[Inet4Address]))
      .set(_.address6).to(InetAddress.getByName("::1").asInstanceOf[Inet6Address])
      .set(_.setAddress6s).to(Set(InetAddress.getByName("::1").asInstanceOf[Inet6Address]))
      .set(_.bigInt).to(BigInt(2) << 128)
      .set(_.listBigInts).to(List(BigInt(2) << 128, BigInt(2) << 256))
      .set(_.bigDecimal).to(BigDecimal(1) / 3)
      .set(_.optBigDecimal).to(Some(BigDecimal(1) / 3))
      .set(_.mapBigDecimalValues).to(Map("a" -> BigDecimal(1) / 3, "b" -> BigDecimal("0.5")))
      .set(_.mapBigDecimalKeys).to(Map(BigDecimal(1) / 3 -> "a", BigDecimal("0.5") -> "b"))
    put(bt)
    val loaded = get("key")
    assert(loaded.isDefined)
    assert(bt === loaded.get)
  }
}
