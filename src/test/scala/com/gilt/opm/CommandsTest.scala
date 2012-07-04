package com.gilt.opm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import java.util

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 7/4/12 1:56 PM
 */

case class HasASet(set: Set[String])
case class HasAMap(map: Map[String, Long])

class CommandsTest extends FunSuite with ShouldMatchers with CommandParser {

  test("test collections") {
    parseAll(command, """set set Set("hello", "world")""").get should equal(
    Command(SetOp, "set", Set("hello", "world")))
    parseAll(command, """set map Map("hello", "world")""").get should equal(
      Command(SetOp, "map", Map("hello" -> "world")))
  }

  test("nested structures") {
    parseAll(command, """set aHasASet com.gilt.opm.HasASet(Set("hello", "world"))""").get should equal(
      Command(SetOp, "aHasASet", HasASet(Set("hello", "world"))))

    parseAll(command, """set aHasAMap com.gilt.opm.HasAMap(Map("hello", 7))""").get should equal(
      Command(SetOp, "aHasAMap", HasAMap(Map("hello" -> 7L))))
  }

  test("parse op") {
    parseAll(op, "set").get should equal(SetOp)
    parseAll(op, "add").get should equal(AddOp)
    parseAll(op, "del").get should equal(DelOp)
  }

  test("command parsing") {
    def assertParses(str: String): Command = {
      parseAll(command, str) match {
        case result@Success(_, _) => println(result); result.get.asInstanceOf[Command]
        case result@Failure(_, _) => fail("Could not parse %s: %s".format(str, result))
      }
    }
    assertParses("set start java.util.Date(Long(1341400386466))") should equal(Command(SetOp, "start", new util.Date(1341400386466L)))
    assertParses("set end None") should equal(Command(SetOp, "end", None))
    assertParses( """add curation com.gilt.opm.Curation(Long(231), List("foo", "bar"))""") should equal(
      Command(AddOp, "curation", Curation(231, Vector("foo", "bar"))))
    assertParses("del curation Long(123)") should equal(Command(DelOp, "curation", 123L))
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
