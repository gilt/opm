package com.gilt.opm

import util.parsing.combinator.JavaTokenParsers
import java.lang.reflect.Method

/*
 *  Some example commands:
 *
 *  set start java.util.Date(Long(1341400386466))
 *  set end None
 *  add curation com.gilt.opm.Curation(Long(231),scala.collection.immutable.Vector(foo, bar))
 *  del curation Long(231)
 *
 *  [op] [on] [deepValue]
 */

sealed trait Operation

object SetOp extends Operation

object AddOp extends Operation

object DelOp extends Operation

trait CommandParser extends JavaTokenParsers {

  def command = op ~ on ~ deepValue

  def op: Parser[Operation] = {
    ("set" | "add" | "del") ^^ {
      case "set" => SetOp
      case "add" => AddOp
      case "del" => DelOp
    }
  }

  def on = super.ident


  def objectName: Parser[AnyRef] = rep1sep(ident, ".") ^^ {
    case name =>
      val clazz = name match {
        case className :: Nil => Class.forName(List("scala", className).mkString(".") + "$")
        case fqcn: List[String] => Class.forName(fqcn.mkString("."))
      }
      clazz.getField("MODULE$").get(clazz)
  }

  def className: Parser[Class[_]] = rep1sep(ident, ".") ^^ {
    case name :: Nil => Class.forName(List("scala", name).mkString("."))
    case fqcn: List[String] => Class.forName(fqcn.mkString("."))
  }

  def constructor: Parser[Any] = className ~ ("(" ~> repsep(deepValue, ",") <~ ")") ^^ {
    case (clazz: Class[_]) ~ (args: List[AnyRef]) =>
      clazz match {
        case long if long.getName == "scala.Long" => args.head.toString.toLong
        case double if double.getName == "scala.Double" => args.head.toString.toDouble
        case str if str eq classOf[String] => args.head.toString
        case _ =>
          clazz.getConstructors.find(_.getParameterTypes.size == args.size) match {
            case Some(con) =>
              val c = con
              c.newInstance(args:_*)
            case None =>
              try {
                val applies = Class.forName(clazz.getName + "$").getMethods.filter(_.getName == "apply")
                val apply = applies.find {
                  case (method: Method) =>
                    method.getParameterTypes.size == 1 && method.getParameterTypes()(0).isAssignableFrom(args.getClass)
                }
                apply.map(_.invoke(null, args)).getOrElse(
                  "Could not find a suitable apply method in %s".format(applies.mkString("\n")))
              } catch {
                case e =>
                  clazz match {
                    case _ if clazz == classOf[Vector[_]] =>
                      Vector() ++ args
                    case _ =>
                      sys.error("No suitable constructor, and no object.apply factory for %s".format(clazz))
                  }
              }
          }
      }
  }

  def deepValue: Parser[Any] =
    (stringLiteralParsed | longLiteralParsed | doubleLiterableParsed | constructor | objectName)

  def doubleLiterableParsed: Parser[Double] = {
    decimalNumber ^^ (_.toDouble)
  }

  def longLiteralParsed: Parser[Long] = {
    wholeNumber ^^ (_.toLong)
  }

  def stringLiteralParsed: Parser[String] =
  // work around bug in superclass (https://issues.scala-lang.org/browse/SI-4138)
    ("\"" + """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""" + "\"").r ^^ {
      case str =>
        val inner = str.tail.dropRight(1)
        var escape = false
        val builder = inner.foldLeft(new StringBuilder) {
          case (b: StringBuilder, c: Char) =>
            if (escape) {
              c match {
                case '\\' => b.append("\\")
                case '\'' => b.append("'")
                case '"' => b.append("\"")
                case 'b' => b.append("\b")
                case 'f' => b.append("\f")
                case 'n' => b.append("\n")
                case 'r' => b.append("\r")
                case 't' => b.append("\t")
                case _ => sys.error("Unknown escape code \\%s in %s".format(c, str))
              }
              escape = false
            } else {
              if (c == '\\') {
                escape = true
              } else {
                b.append(c)
              }
            }
            b
        }
        builder.toString()
    }
}
