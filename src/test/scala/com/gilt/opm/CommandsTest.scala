package com.gilt.opm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 7/4/12 1:56 PM
 */

class CommandsTest extends FunSuite with ShouldMatchers with CommandParser {

  test("parse op") {
    parseAll(op, "set").get should equal(SetOp)
    parseAll(op, "add").get should equal(AddOp)
    parseAll(op, "del").get should equal(DelOp)
  }

  test("command parsing") {
    def assertParses(str: String) {
      println("Parsing %s".format(str))
      parseAll(command, str) match {
        case result@Success(_, _) => println(result)
        case result@Failure(_, _) => fail("Could not parse %s: %s".format(str, result))
      }
    }
    assertParses("set start java.util.Date(Long(1341400386466))")
    assertParses("set end None")
    assertParses("""add curation com.gilt.opm.Curation(Long(231), scala.collection.immutable.Vector("foo", "bar"))""")
    assertParses("del curation Long(123)")
  }

  test("string literal parsing") {
    parseAll(stringLiteralParsed,
      """
        |"hello, world"
      """.stripMargin).get should equal("hello, world")
    parseAll(stringLiteralParsed,
      """
        |"'now is the\ntime for all good\n\"men\" to come\tto the aid of their country"
      """.stripMargin).get should equal(
      "'now is the\ntime for all good\n\"men\" to come\tto the aid of their country")
  }

  test("long literal parsing") {
    parseAll(longLiteralParsed, "123").get should equal(123L)
    parseAll(longLiteralParsed, Long.MaxValue.toString).get should equal(Long.MaxValue)
    parseAll(longLiteralParsed, Long.MinValue.toString).get should equal(Long.MinValue)
  }

  test("double literal parsing") {
    parseAll(doubleLiterableParsed, "1.23").get should equal("1.23".toDouble)
    parseAll(doubleLiterableParsed, "0.00").get should equal("0".toDouble)
  }

  test("object discovery") {
    parseAll(objectName, "None").get should equal(None)
  }
}
