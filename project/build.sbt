resolvers ++= Seq(
    Resolver.url("gilt-plugin-snapshots", new URL("https://nexus.gilt.com/nexus/content/repositories/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns),
    Resolver.url("gilt-plugin-releases", new URL("https://nexus.gilt.com/nexus/content/repositories/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
)

addSbtPlugin("com.giltgroupe" % "gilt-sbt-build" % "0.0.12-SNAPSHOT")

