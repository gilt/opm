
resolvers += "gilt.snapshots" at "https://nexus.gilt.com/nexus/content/repositories/gilt.snapshots"

resolvers += "gilt.releases" at "https://nexus.gilt.com/nexus/content/repositories/internal-releases-pom"

libraryDependencies ++= Seq(
    "javax.mail" % "mail" % "1.4.5",
    "com.giltgroupe" %% "gilt-sbt-build" % "0.0.3"
)

resolvers += Resolver.url(
  "sbt-plugin-releases",
    new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/")
)(Resolver.ivyStylePatterns)

resolvers += Resolver.url(
   "sbt-plugin-snapshots",
   new URL("https://nexus.gilt.com/nexus/content/repositories/sbt-plugin-snapshots/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.6.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.6-SNAPSHOT")
