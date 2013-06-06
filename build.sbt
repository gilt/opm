gilt.GiltProject.jarSettings

name := "opm"

organization := "com.giltgroupe"

crossScalaVersions in ThisBuild := Seq("2.9.1", "2.9.2")

libraryDependencies ++= Seq(
  "com.giltgroupe" %% "commons-util" % "4.5.3",
  "org.mongodb" %% "casbah-core" % "2.6.1",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

