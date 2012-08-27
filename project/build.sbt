resolvers ++= Seq(
  Resolver.url("sbt-plugin-snapshots",new URL("https://nexus.gilt.com/nexus/content/repositories/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns)
  )

addSbtPlugin("com.giltgroupe" % "gilt-sbt-build" % "0.0.7-SNAPSHOT")

