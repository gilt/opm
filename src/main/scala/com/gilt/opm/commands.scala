package com.gilt.opm

import util.parsing.combinator.JavaTokenParsers
import java.lang.reflect.Method

/*
 *  Some example commands:
 *
 *  set start java.util.Date(Long(1341400386466))
 *  set end None
 *  set curation com.gilt.opm.Curation(Long(231),scala.collection.immutable.Vector("foo", "bar"))
 *  set curation Long(231)
 */

sealed trait Operation

object CreateOp extends Operation

object SetOp extends Operation

case class Command(op: Operation, field: Option[String], value: Any)

trait CommandParser extends JavaTokenParsers {

  def create: Parser[Command] = "create" ~> className ^^ {
    case toInstantiate: Class[_] => Command(CreateOp, None, toInstantiate)
  }

  def mutate: Parser[Command] = (op ~ on ~ deepValue) ^^ {
    case op ~ on ~ deepValue => Command(op, Some(on), deepValue)
  }

  def command = create | mutate

  def op: Parser[Operation] = "set" ^^ (_ => SetOp)

  def on = super.ident      // for now this is just a field name; maybe someday we can do foo.bar.zip.zap

  def objectName: Parser[AnyRef] = rep1sep(ident, ".") ^^ {
    case name =>
      val clazz = name match {
        case className :: Nil =>
          Class.forName(List("scala", className).mkString(".") + "$")
        case fqcn: List[_] =>
          Class.forName(fqcn.mkString(".") + "$")
      }
      clazz.getField("MODULE$").get(clazz)
  }

  def className: Parser[Class[_]] = rep1sep(ident, ".") ^^ {
    case "List" :: Nil => classOf[scala.collection.immutable.Vector[_]]
    case "Set" :: Nil => classOf[scala.collection.immutable.Set[_]]
    case "Map" :: Nil => classOf[scala.collection.immutable.Map[_, _]]
    case name :: Nil => Class.forName(List("scala", name).mkString("."))
    case fqcn: List[_] => Class.forName(fqcn.mkString("."))
  }

  def constructor: Parser[Any] = className ~ ("(" ~> repsep(deepValue, ",") <~ ")") ^^ {
    case (clazz: Class[_]) ~ (args: List[_]) =>
      clazz match {
        // for some reason, class equality for classes like scala.Long is not reliable,
        // so we fall back to just checking the name. scala bug? todo
        case long if long.getName == "scala.Long" => args.head.toString.toLong
        case double if double.getName == "scala.Double" => args.head.toString.toDouble
        case str if str eq classOf[String] => args.head.toString
        case _ =>
          // todo if several match then look for one with matching argument types
          clazz.getConstructors.find(_.getParameterTypes.size == args.size) match {
            case Some(con) =>
              val c = con
              c.newInstance(args.map(_.asInstanceOf[AnyRef]): _*)
            case None =>
              // no suitable constructor; look for an apply method.  For now we only look for apply methods
              // that take a single Seq-like argument; this could be enhanced. todo
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
                    case _ if clazz == classOf[Set[_]] =>
                      Set() ++ args
                    case _ if clazz == classOf[Map[_, _]] =>
                      Map() ++ args.grouped(2).map(pair => (pair(0), pair(1)))
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
