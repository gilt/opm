import scoverage.ScoverageKeys

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8")

scalacOptions := Seq("-deprecation", "-feature")

ScoverageKeys.coverageFailOnMinimum := true

ScoverageKeys.coverageMinimum := 91.3

libraryDependencies ++= Seq(
  "com.gilt" %% "gfc-util" % "0.1.7",
  "com.gilt" %% "gfc-time" % "0.0.7",
  "com.gilt" %% "gfc-id" % "0.0.7",
  "com.gilt" %% "gfc-logging" % "0.0.7",
  "org.mongodb" %% "casbah-core" % "2.7.4",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.6.3",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.7" % "test"
)

resolvers += "Typesafe releases" at "https://repo.typesafe.com/typesafe/releases"
