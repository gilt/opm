package com.gilt.opm

import org.scalatest.FunSuite

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 2/3/13 5:08 PM
 */
object MapTests {

  trait MapObject extends OpmObject {
    def map: Map[String, Long]
  }
}

class MapTests extends FunSuite with OpmMongoStorage[MapTests.MapObject] with CollectionHelper {
  def collectionName = "opm-MapTests"

  import MapTests.MapObject
  import OpmFactory._

  test("simple map-based object can be stored & retrieved") {
    val mapObj = instance[MapObject]("a", Map("map" -> Map.empty))

    val obj = mapObj.set(_.map) := Map("now" -> System.currentTimeMillis(), "then" -> (System.currentTimeMillis - 10))

    put(obj)

    val loaded = get("a").get

    val theMap = loaded.map
    assert(theMap("now") <= System.currentTimeMillis())
    assert(theMap("now") === theMap("then") + 10)
  }
}
