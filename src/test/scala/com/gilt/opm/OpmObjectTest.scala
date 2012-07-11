package com.gilt.opm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 7/11/12 10:55 AM
 */

object OpmObjectTest {
  trait OpmTestObject extends OpmObject {
    def foo: String
  }

  trait NativeTypes extends OpmObject {
    def aByte: Byte
    def aChar: Char
    def aShort: Short
    def aInt: Int
    def aLong: Long
    def aFloat: Float
    def aDouble: Double
  }
}

class OpmObjectTest extends FunSuite with ShouldMatchers {
  import OpmObjectTest._
  import OpmFactory._
  test("magic") {
    // confirm calling magic always sucks
    val testObj = instance[OpmTestObject]
    evaluating(testObj.magic()) should produce[RuntimeException]
    evaluating(new OpmObject{}.magic()) should produce[RuntimeException]
  }

  test("native types") {
    val fixture = instance[NativeTypes].
      set(_.aByte).to(1.asInstanceOf[Byte]).
      set(_.aChar).to(2.asInstanceOf[Char]).
      set(_.aShort).to(3.asInstanceOf[Short]).
      set(_.aInt).to(4).
      set(_.aLong).to(5.asInstanceOf[Long]).
      set(_.aFloat).to(6.asInstanceOf[Float]).
      set(_.aDouble).to(7.asInstanceOf[Double])

    assert(fixture.aByte === 1.asInstanceOf[Byte])
    assert(fixture.aChar === 2.asInstanceOf[Char])
    assert(fixture.aShort === 3.asInstanceOf[Short])
    assert(fixture.aInt === 4)
    assert(fixture.aLong === 5.asInstanceOf[Long])
    assert(fixture.aFloat === 6.asInstanceOf[Float])
    assert(fixture.aDouble === 7.asInstanceOf[Double])
  }

  test("equality & hashCode") {
    val foo = instance[OpmTestObject].set(_.foo) := "foo"
    assert(foo === foo)
    assert(instance[OpmTestObject].set(_.foo).to("hello") === instance[OpmTestObject].set(_.foo).to("hello"))
    assert(foo != instance[OpmTestObject].set(_.foo).to("FOO"))
    assert(foo != 7)

    val first = OpmFactory.instance[OpmTestObject].set(_.foo).to("initialValue")
    val second = first.set(_.foo).to("secondValue")
    val third = second.set(_.foo).to("initialValue")
    assert(first === third)
    assert(first.hashCode === third.hashCode)
  }

  test("special methods") {
    // methods on object: notify,wait,notifyAll,toString,getClass,hashCode
    // of these, notify, wait, notifyAll, & getClass are final.
    // so it remains to understand what of toString and hashCode
    val foo = instance[OpmTestObject]
    assert(foo.toString contains "OpmTestObject")
  }
}
