gilt.GiltProject.jarSettings

gilt.GiltProject.forkSourceSettings

name := "opm"

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.2")

libraryDependencies ++= Seq(
  "com.giltgroupe" %% "commons-util" % "4.7.6.20130826174027",
  "org.mongodb" %% "casbah-core" % "2.6.1",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)
