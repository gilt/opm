package opm

import sbt._

object OpmBuild extends Build {

  lazy val Default = config("default") describedAs("for legacy compatibility") extend(Compile)

  lazy val root = Project(
    id = "opm",
    base = file("."),
    settings = Project.defaultSettings
  ).configs(Default)

}

