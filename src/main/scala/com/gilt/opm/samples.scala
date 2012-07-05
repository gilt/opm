package com.gilt.opm

import java.util

case class Curation(id: Long, lookElements: Seq[String]) extends Mutatable[Curation] with HasId {
  def this() = this(0l, Nil)
}

case class Sale(id: Long, curation: Seq[Curation], start: util.Date, end: Option[util.Date]) extends Mutatable[Sale] with HasId {
  def this() = this(0l, Nil, new util.Date(0), Some(new util.Date(0)))
}

