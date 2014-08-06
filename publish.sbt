
publishMavenStyle := true

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test in ThisBuild := false

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild := (
  <scm>
    <url>git@github.com:gilt/opm.git</url>
    <connection>scm:git:git@github.com:gilt/opm.git</connection>
  </scm>
  <developers>
    <developer>
      <id>ebowman</id>
      <name>Eric Bowman</name>
      <email>ebowman@gilt.com</email>
      <url>https://twitter.com/ebowman</url>
    </developer>
    <developer>
      <id>rmartin</id>
      <name>Ryan Martin</name>
      <email>rmartin@gilt.com</email>
    </developer>
    <developer>
      <id>ssullivan</id>
      <name>Sean Sullivan</name>
      <email>ssullivan@gilt.com</email>
    </developer>
    <developer>
      <id>aduffy</id>
      <name>Andrew Duffy</name>
      <email>aduffy@gilt.com</email>
    </developer>
    <developer>
      <id>khyland</id>
      <name>Kevin Hyland</name>
      <email>khyland@gilt.com</email>
    </developer>
    <developer>
      <id>rbrazier</id>
      <name>Robert Brazier</name>
      <email>rbrazier@gilt.com</email>
    </developer>
    <developer>
      <id>akaplan</id>
      <name>Adam Kaplan</name>
      <email>akaplan@gilt.com</email>
    </developer>
    <developer>
      <id>grhodes</id>
      <name>Graham Rhodes</name>
      <email>grhodes@gilt.com</email>
    </developer>
    <developer>
      <id>mseok</id>
      <name>Myyk Seok</name>
      <email>mseok@gilt.com</email>
    </developer>
  </developers>)
