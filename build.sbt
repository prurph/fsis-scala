scalaVersion := "2.11.4"

resolvers ++= Seq(
  "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
addCompilerPlugin("org-spire-math" % "kind-projector_2.11" % "0.5.2")

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.3.0"

