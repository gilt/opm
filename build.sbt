scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.10.4", "2.11.4")

scalacOptions := Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "com.gilt" %% "gfc-util" % "0.0.3",
  "com.gilt" %% "gfc-time" % "0.0.3",
  "com.gilt" %% "gfc-id" % "0.0.2",
  "org.mongodb" %% "casbah-core" % "2.7.3",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.4.0",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.1" % "test"
)

instrumentSettings

ScoverageKeys.highlighting := true

resolvers += "Typesafe releases" at "https://repo.typesafe.com/typesafe/releases"
