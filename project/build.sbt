
resolvers ++= Seq(
  Resolver.url("bootstrap-ivy",
    new URL("https://nexus.gilt.com/nexus/content/groups/bootstrap/"))(Resolver.ivyStylePatterns),
  "bootstrap-pom" at "https://nexus.gilt.com/nexus/content/groups/bootstrap"
)

addSbtPlugin("com.giltgroupe" % "gilt-sbt-build" % "0.0.30")
