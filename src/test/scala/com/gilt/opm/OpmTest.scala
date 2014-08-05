package com.gilt.opm

import org.scalatest.FunSuite
import org.scalatest.Matchers
import java.util.concurrent.TimeUnit
import java.util.NoSuchElementException
import java.util
import scala.collection.JavaConverters._

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 7/9/12 1:58 PM
 */

object OpmTest {

  trait Named extends OpmObject {
    def name: String
    def optName: Option[String] = None
  }

  trait Foo extends Named {
    def id: Long

    def bar: Option[Bar]
  }

  trait SubFoo extends Foo {}

  trait Bar extends Named {
    def things: Set[String]
  }

  trait Baz extends OpmObject {
    def id: Long = 0

    def things: Set[String]

    def jThings: util.Set[String]

    def thingsMap: Map[String, String]

    def jThingsMap: util.Map[String, String]
  }

  trait MixOfOptionalNonOptional extends Named {
    def value: Option[String]
  }

  trait MixOfOptionalNonOptionalWithMethod extends Named {
    def value: Option[String]

    override def equals(obj: Any) = obj match {
      case (o: MixOfOptionalNonOptionalWithMethod) =>
        (this.name == o.name) &&
        (this.value == o.value)
      case _ => false
    }
  }
}

class OpmTest extends FunSuite with Matchers {

  import OpmTest.{Foo, Bar, Baz, SubFoo, MixOfOptionalNonOptional, MixOfOptionalNonOptionalWithMethod, Named}
  import OpmFactory._

  var time = 0l

  test("basics") {
    val foo = instance[Foo]("")
    val foo2 = foo.set(_.name) := "test"
    assert(foo2.name === "test")

    val foo3 = foo2.set(_.bar).to(Some(instance[Bar]("")))
    val foo4 = foo3.set(_.bar.get.name).to("a bar")

    assert(foo4.bar.get.name === "a bar")

    val foo5 = foo4.set(_.bar.get.things) to Set("a,b,c".split(","): _*)
    assert(foo5.bar.get.things === Set("a", "b", "c"))
  }

  test("set id") {
    var foo = instance[Foo]("")
    foo = foo.set(_.id) := 7L
    assert(foo.id === 7L)
  }

  test("prune") {
    val empty = instance[Foo]("")
    val init = empty set (_.name) := "init"
    val pruned = init.set(_.name).to("ready").prune
    assert(pruned.timeline.size === 1)
  }

  test("forceCurrent") {
    val a = instance[Foo]("").set(_.name).to("a").prune
    val b = a.set(_.name) := "b"
    val c = b.set(_.name) := "c"
    val bp = b.forceAfter(c)
    assert(bp === b)
    assert(bp.timeline.head === bp)
    assert(bp.timeline.drop(2).take(1).head === b)

    val cp = c :: bp
    assert(cp.timeline.take(3).toList === List(cp, c, bp))
  }

  test("timestamp") {
    val before = OpmFactory.clock()
    val a = instance[Foo]("").set(_.name) := "a"
    val after = OpmFactory.clock()
    assert(a.opmTimestamp >= before && a.opmTimestamp <= after)
  }

  test("branching") {
    val a = instance[Foo]("").set(_.name).to("a").prune
    val b = a.set(_.name) := "b"
    val c = b.set(_.name) := "c"
    val d = c.set(_.name) := "d"
    val e = d.set(_.name) := "e"

    val f = c.set(_.name) := "f"
    val g = f.set(_.name) := "g"

    assert(e.timeline.toList === List(e, d, c, b, a))
    assert(g.timeline.toList === List(g, f, c, b, a))
  }

  test("searching history") {
    var foo = instance[Foo]("").set(_.name).to("a").prune
    foo = foo.set(_.name) := "b"
    val beforeC = clock()
    foo = foo.set(_.name) := "c"
    foo = foo.set(_.name) := "d"

    val b = foo.timeline.dropWhile(_.opmTimestamp >= beforeC)
    assert(b.head === instance[Foo]("").set(_.name).to("b"))
  }

  test("basic diff & evolution") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l, "bar" -> instance[Bar]("", Map("name" -> "barA", "things" -> Set("keg", "glass", "stool")))))
    val b = instance[Foo]("", Map("name" -> "b", "id" -> 1l, "bar" -> instance[Bar]("", Map("name" -> "barB", "things" -> Set("ken", "malibu cruiser")))))

    val fromAtoB = b diff a
    val bp = a evolve fromAtoB
    assert(bp === b)
  }

  test("basic diff & evolution, direct call") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l, "bar" -> instance[Bar]("", Map("name" -> "barA", "things" -> Set("keg", "glass", "stool")))))
    val b = instance[Foo]("", Map("name" -> "b", "id" -> 1l, "bar" -> instance[Bar]("", Map("name" -> "barB", "things" -> Set("ken", "malibu cruiser")))))

    val bp = a evolve b
    assert(bp === b)
  }

  test("diff from builder object, then evolution") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l, "bar" -> instance[Bar]("", Map("name" -> "barA", "things" -> Set("keg", "glass", "stool")))))
    val b = instance[Foo]().set(_.id).to(1l)

    val bp = a evolve b
    assert(bp.id === 1l)
    assert(bp.name === "a")
  }

  test("evolve removing a field") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l, "bar" -> instance[Bar]("", Map("name" -> "bar", "things" -> Set("pub")))))
    val b = instance[Foo]("", Map("name" -> "b", "id" -> 1l))

    val fromAtoB = b diff a
    val bp = a evolve fromAtoB
    assert(bp === b)
  }

  test("evolve adding a field") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l))
    val b = instance[Foo]("", Map("name" -> "b", "id" -> 1l, "bar" -> instance[Bar]("", Map("name" -> "bar", "things" -> Set("pub")))))

    val fromAtoB = b diff a
    val bp = a evolve fromAtoB
    assert(bp === b)
  }

  test("identical evolution") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l))
    val b = a

    val fromAtoB = b diff a
    val bp = a evolve fromAtoB
    assert(bp === b)

  }

  test("empty evolution") {
    val a = instance[Foo]("")
    val b = instance[Foo]("")

    val fromAtoB = b diff a
    val bp = a evolve fromAtoB
    assert(bp === b)
  }

  // test diff across types fails
  test("diff type constraints") {
    a [RuntimeException] should be thrownBy {
      instance[Foo]("") diff
        new Bar {
          def name = null

          def things = Set.empty[String]
        }.asInstanceOf[Foo]
    }
    a [RuntimeException] should be thrownBy {
      instance[Foo]("") diff instance[Bar]("")
    }
  }

  // test diff across sub-types should not fail
  test("diff sub-type is possible") {
    instance[Foo]("") diff instance[SubFoo]("")
  }

  test("evolve from trait only") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l, "bar" -> instance[Bar]("", Map("name" -> "barA", "things" -> Set("keg", "glass", "stool")))))
    val b = instance[Named]().set(_.name).to("b")

    val bp = a evolve b
    assert(bp.id === 0l)
    assert(bp.name === "b")
  }

  test("required fields present") {
    instance[MixOfOptionalNonOptional]("", Map("name" -> "eric"))
    instance[MixOfOptionalNonOptional]("", Map("name" -> "eric", "value" -> Some("skibum") ))
    an [IllegalArgumentException] should be thrownBy {
      instance[MixOfOptionalNonOptional]("", Map("value" -> Some("skibum") ))
    }
  }

  test("required fields ignores methods") {
    instance[MixOfOptionalNonOptionalWithMethod]("", Map("name" -> "eric"))
    instance[MixOfOptionalNonOptionalWithMethod]("", Map("name" -> "eric", "value" -> Some("skibum") ))
    an [IllegalArgumentException] should be thrownBy {
      instance[MixOfOptionalNonOptionalWithMethod]("", Map("value" -> Some("skibum") ))
    }
  }

  test("nested option"){
    val foo = instance[Foo]("fake").set(_.bar).to(None)
    assert(foo.bar === None)
    val foo2 = instance[Foo]("fake").set(_.bar).to(Option(null))
    assert(foo2.bar === None)
  }

  test("nested option reads None when not set"){
    val foo = instance[Foo]("fake").set(_.id).to(1)
    assert(foo.id === 1)
    assert(foo.bar === None)
  }

  test("nested iterable reads Empty when not set"){
    val baz = instance[Baz]("fake").set(_.things).to(Set.empty)
    assert(baz.things === Set.empty)

    val baz2 = instance[Baz]("fake").set(_.id).to(1)
    assert(baz2.id === 1)
    assert(baz2.things === Set.empty)
    assert(baz2.jThings === Set.empty.asJava)
  }

  test("edit nested iterable"){
    val baz = instance[Baz]("fake").set(_.things).to(Set("1", "2"))
    assert(baz.things === Set("1", "2"))

    val updatedBaz = baz.set(_.things).to(baz.things ++ Set("3", "4"))
    assert(updatedBaz.things === Set("1", "2", "3", "4"))

    val list = new util.TreeSet[String]()
    list.add("1")
    list.add("2")
    val baz1 = instance[Baz]("fake").set(_.jThings).to(list)
    assert(baz1.jThings.size === 2)
    assert(baz1.jThings.toArray === Array("1", "2"))

    val updated = new util.TreeSet[String](baz1.jThings)
    updated.add("3")
    updated.add("4")
    val updatedBaz1 = baz1.set(_.jThings).to(updated)
    assert(updatedBaz1.jThings.size === 4)
    assert(updatedBaz1.jThings.toArray === Array("1", "2", "3", "4"))
  }

  test("nested map"){
    val baz = instance[Baz]("fake").set(_.thingsMap).to(Map.empty)
    assert(baz.thingsMap === Map.empty)

    val baz2 = instance[Baz]("fake").set(_.thingsMap).to(Map("1" -> "2"))
    assert(baz2.thingsMap === Map("1" -> "2"))

    val baz3 = instance[Baz]("fake").set(_.jThingsMap).to(Map.empty[String, String].asJava)
    assert(baz3.jThingsMap === Map.empty[String, String].asJava)

    val baz4 = instance[Baz]("fake").set(_.jThingsMap).to(Map("1" -> "2").asJava)
    assert(baz4.jThingsMap.size === 1)
    assert(baz4.jThingsMap.get("1") === "2")

    val baz5 = instance[Baz]("fake").set(_.jThingsMap).to(new util.HashMap[String, String]())
    assert(baz5.jThingsMap === new util.HashMap[String, String]())

    val hm = new util.HashMap[String, String]()
    hm.put("1", "2")
    val baz6 = instance[Baz]("fake").set(_.jThingsMap).to(hm)
    assert(baz6.jThingsMap.size === 1)
    assert(baz6.jThingsMap.get("1") === "2")
  }

  test("edit nested map"){
    val baz = instance[Baz]("fake").set(_.thingsMap).to(Map("1" -> "2"))
    assert(baz.thingsMap === Map("1" -> "2"))

    val updatedBaz = baz.set(_.thingsMap).to(baz.thingsMap ++ Map("3" -> "4"))
    assert(updatedBaz.thingsMap === Map("1" -> "2", "3" -> "4"))

    val hm = new util.HashMap[String, String]()
    hm.put("1", "2")
    val baz1 = instance[Baz]("fake").set(_.jThingsMap).to(hm)
    assert(baz1.jThingsMap.size === 1)
    assert(baz1.jThingsMap.get("1") === "2")

    val updated: util.HashMap[String, String] = baz1.jThingsMap.asInstanceOf[util.HashMap[String, String]]
    updated.put("3", "4")
    val updatedBaz1 = baz1.set(_.jThingsMap).to(updated)
    assert(updatedBaz1.jThingsMap.size === 2)
    assert(updatedBaz1.jThingsMap.get("1") === "2")
    assert(updatedBaz1.jThingsMap.get("3") === "4")
  }

  test("nested map reads Empty when not set"){
    val baz = instance[Baz]("fake").set(_.id).to(1)
    assert(baz.id === 1)
    assert(baz.thingsMap === Map.empty)
    assert(baz.jThingsMap === Map.empty[String, String].asJava)
  }

  test("nested non-option throws an exception when not set"){
    val foo = instance[Foo]("fake")
    intercept[NoSuchElementException] {
      assert(foo.id === 1)
    }
  }

  test("set pending") {
    val a = instance[Foo]().
      set(_.id).to(1L).
      set(_.name).toPending(500, TimeUnit.MILLISECONDS)
    an [PendingOpmValueException] should be thrownBy {
      a.name
    }
    assert(a.isPending(_.name))
  }

  test("remove pending") {
    val a = instance[Foo]().
      set(_.id).to(1L).
      set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    assert(a.isPending(_.name))
    val b = a.set(_.name).toNotPending()
    assert(!b.isPending(_.name))
  }

  test("remove pending on set field does nothing") {
    val a = instance[Foo]().
      set(_.id).to(1L).
      set(_.name).to("name1")
    assert(!a.isPending(_.name))
    val b = a.set(_.name).toNotPending()
    assert(!b.isPending(_.name))
  }

  test("set pending when previous value is None") {
    val a = instance[Foo]().
      set(_.id).to(1L).
      set(_.optName).to(None).
      set(_.optName).toPending(500, TimeUnit.MILLISECONDS)
    an [PendingOpmValueException] should be thrownBy {
      a.optName
    }
    assert(a.isPending(_.optName))
  }

  test("pending should expire") {
    val a = instance[Foo]().
      set(_.id).to(1L).
      set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    Thread.sleep(101)
    an [NoSuchElementException] should be thrownBy {
      a.name
    }
    assert(!a.isPending(_.name))
  }

  test("pending should clear when value is set") {
    val a = instance[Foo]().
      set(_.id).to(1L).
      set(_.name).toPending(1000, TimeUnit.MILLISECONDS)
    an [PendingOpmValueException] should be thrownBy {
      a.name
    }
    val b = a.set(_.name).to("name1")
    assert(b.name === "name1")
    assert(!b.isPending(_.name))
  }

  test("don't set pending on field that has already been set to a value") {
    val a = instance[Foo]().
      set(_.name).to("a")
    val b = a.set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    assert(b.name === "a")
    assert(a.opmTimestamp == b.opmTimestamp)
    assert(!b.isPending(_.name))
  }

  test("don't set pending on field that has already been set to pending") {
    val a = instance[Foo]().
      set(_.name).toPending(500, TimeUnit.MILLISECONDS)
    Thread.sleep(10)
    val b = a.set(_.name).toPending(10, TimeUnit.SECONDS)
    assert(a.opmTimestamp == b.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy {
      b.name
    }
    Thread.sleep(500)
    an [NoSuchElementException] should be thrownBy b.name
  }

  test("set pending on field on which previous pending has expired") {
    val a = instance[Foo]().
      set(_.name).toPending(500, TimeUnit.MILLISECONDS)
    Thread.sleep(10)
    val b = a.set(_.name).toPending(10, TimeUnit.SECONDS)
    assert(a.opmTimestamp == b.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy b.name
    Thread.sleep(500)
    an [NoSuchElementException] should be thrownBy b.name
    val c = a.set(_.name).toPending(500, TimeUnit.MILLISECONDS)
    assert(a.opmTimestamp != c.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy c.name
    val d = b.set(_.name).toPending(500, TimeUnit.MILLISECONDS)
    assert(b.opmTimestamp != d.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy d.name
  }

  test("don't set pending on field that has already been set to pending then set a value") {
    val a = instance[Foo]().
      set(_.name).toPending(1000, TimeUnit.MILLISECONDS)
    an [PendingOpmValueException] should be thrownBy a.name
    val b = a.set(_.name).to("name1")
    assert(b.name === "name1")
    val c = b.set(_.name).toPending(1000, TimeUnit.MILLISECONDS)
    assert(c == b)
    assert(c.opmTimestamp == b.opmTimestamp)
  }

  test("evolve from pending trait") {
    val a = instance[Foo]().set(_.id).to(0l)
    val b = instance[Named]().set(_.name).toPending(100, TimeUnit.MILLISECONDS)

    val bp = a evolve b
    assert(bp.id === 0l)

    an [PendingOpmValueException] should be thrownBy bp.name
    assert(bp.isPending(_.name))
    Thread.sleep(100)
    an [NoSuchElementException] should be thrownBy bp.name
    assert(!bp.isPending(_.name))
  }

  test("evolve from pending optional trait set to None") {
    val a = instance[Foo]().set(_.id).to(0l).set(_.optName).to(None)
    val b = instance[Named]().set(_.optName).toPending(100, TimeUnit.MILLISECONDS)

    val bp = a evolve b
    assert(bp.id === 0l)

    an [PendingOpmValueException] should be thrownBy bp.optName
    Thread.sleep(100)
    an [NoSuchElementException] should be thrownBy bp.optName
  }

  test("evolve from pending sets value when set") {
    val a = instance[Foo]().set(_.id).to(0l).set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    val b = instance[Named]().set(_.name).to("name1")

    an [PendingOpmValueException] should be thrownBy a.name
    val bp = a evolve b
    assert(bp.id === 0l)
    assert(bp.name === "name1")
    assert(bp.opmTimestamp != a.opmTimestamp)
    assert(!bp.isPending(_.name))
  }

  test("evolve from pending trait does nothing when already set") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l, "bar" -> instance[Bar]("", Map("name" -> "barA", "things" -> Set("keg", "glass", "stool")))))
    val b = instance[Named]().set(_.name).toPending(100, TimeUnit.MILLISECONDS)

    val bp = a evolve b
    assert(bp.id === 0l)
    assert(bp.name === "a")
    assert(bp.opmTimestamp == a.opmTimestamp)
    assert(!bp.isPending(_.name))
  }

  test("evolve to not pending") {
    val a = instance[Foo]().set(_.id).to(0l).set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    val b = instance[Named]().set(_.name).toNotPending()

    val bp = a evolve b
    assert(bp.id === 0l)
    assert(bp.opmTimestamp != a.opmTimestamp)
    assert(!bp.isPending(_.name))
  }

  test("evolve to not pending does nothing when already set") {
    val a = instance[Foo]("", Map("name" -> "a", "id" -> 0l, "bar" -> instance[Bar]("", Map("name" -> "barA", "things" -> Set("keg", "glass", "stool")))))
    val b = instance[Named]().set(_.name).toNotPending()

    val bp = a evolve b
    assert(bp.id === 0l)
    assert(bp.name === "a")
    assert(bp.opmTimestamp == a.opmTimestamp)
    assert(!bp.isPending(_.name))
  }

  test("evolve from pending trait does nothing when already pending") {
    val a = instance[Foo]().set(_.id).to(0l).set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    val b = instance[Named]().set(_.name).toPending(10, TimeUnit.SECONDS)

    val bp = a evolve b
    assert(bp.id === 0l)
    assert(bp.opmTimestamp == a.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy bp.name
    Thread.sleep(100)
    an [NoSuchElementException] should be thrownBy bp.name
  }

  test("evolve from pending trait sets pending when previous pending has expired") {
    val a = instance[Foo]().set(_.id).to(0l).set(_.name).toPending(100, TimeUnit.MILLISECONDS)
    val b = instance[Named]().set(_.name).toPending(10, TimeUnit.SECONDS)

    val c = a evolve b
    assert(c.id === 0l)
    assert(c.opmTimestamp == a.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy c.name
    Thread.sleep(100)
    an [NoSuchElementException] should be thrownBy c.name
    val d = a evolve b
    assert(d.opmTimestamp != a.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy d.name
    val e = c evolve b
    assert(e.opmTimestamp != c.opmTimestamp)
    an [PendingOpmValueException] should be thrownBy e.name
  }
}
