package opm

import sbt._
import gilt.GiltProject

object OpmBuild extends Build {
  lazy val root = GiltProject(id = "opm", base = file("."))
}

