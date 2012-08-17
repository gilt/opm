package opm

import sbt._

object OpmBuild extends gilt.GiltJarBuild {

  lazy val root = Project(
    id = "opm",
    base = file("."),
    settings = Project.defaultSettings ++ baseSettings
  )
}

