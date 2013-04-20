name := "gpp"

version := "0.1"

organization := "edu.utexas"

scalaVersion := "2.10.1"

crossPaths := false

retrieveManaged := true

resolvers ++= Seq(
  "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalanlp" % "nak" % "1.1.2",
  "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT",
  "org.rogach" %% "scallop" % "0.8.1",
  "org.clapper" % "argot_2.9.1" % "0.3.8",
  "gov.nist.math" % "jama" % "1.0.2" 
)
