resolvers += Resolver.url(
    "sbt-plugin-snapshots",
    new URL("https://nexus.gilt.com/nexus/content/repositories/sbt-plugin-releases/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.giltgroupe" % "gilt-sbt-build" % "0.0.10")

