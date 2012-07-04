name := "opm"

version := "1.0"

scalaVersion := "2.9.2"

resolvers ++= Seq(
        "sonatype oss" at "https://oss.sonatype.org/content/repositories/releases/"
        )

libraryDependencies ++= Seq(
        "ch.qos.logback" % "logback-classic" % "0.9.28",
        "ch.qos.logback" % "logback-core" % "0.9.28",
        "org.scalatest" %% "scalatest" % "1.8" % "test",
        "org.scalacheck" %% "scalacheck" % "1.9" % "test"
        )

scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8")

javacOptions ++= Seq("-Xlint:deprecation", "-encoding", "utf8")
