package com.gilt

package object opm {
  // These are the same, but it feels better to have separate names
  type OpmToMongoMapper = Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]]
  type OpmFromMongoMapper = Option[PartialFunction[(String, Option[Class[_]], AnyRef), AnyRef]]
}
