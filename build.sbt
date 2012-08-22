name := "opm"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
        "org.slf4j" % "slf4j-api" % "1.6.6",
        "org.slf4j" % "slf4j-log4j12" % "1.6.6" % "provided",
        "log4j" % "log4j" % "1.2.17" % "provided",
        "org.scalatest" %% "scalatest" % "1.8" % "test",
        "org.scalacheck" %% "scalacheck" % "1.9" % "test"
        )

crossPaths := false

