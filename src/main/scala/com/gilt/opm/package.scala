package com.gilt

package object opm {
  // These are the same, but it feels better to have separate names
  type OpmToMongoMapper = PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]
  type OpmFromMongoMapper = PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]
  val noOpMongoMapper = new PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef] {
    def apply(args: (String, Option[Class[_]], AnyRef)) = throw new Exception("This is a no-op, you should not get here.")
    def isDefinedAt(args: (String, Option[Class[_]], AnyRef)) = false
  }
}
