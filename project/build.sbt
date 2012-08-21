resolvers ++= Seq(
  "gilt.snapshots" at "https://nexus.gilt.com/nexus/content/repositories/gilt.snapshots",
  "gilt.releases" at "https://nexus.gilt.com/nexus/content/repositories/internal-releases-pom",
  Resolver.url("sbt-plugin-releases",new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
  Resolver.url("sbt-plugin-snapshots",new URL("https://nexus.gilt.com/nexus/content/repositories/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns)
  )

libraryDependencies ++= Seq(
    "com.giltgroupe" %% "gilt-sbt-build" % "0.0.4-SNAPSHOT"
)

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.6.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.6-SNAPSHOT")

