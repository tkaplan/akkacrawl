name := "wiki"
version := "0.1.0"
scalaVersion := "2.11.8"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor_2.11" % "2.4.12",
  "com.typesafe.akka" % "akka-remote_2.11" % "2.4.12",
  "com.typesafe.akka" % "akka-testkit_2.11" % "2.4.12",
  "com.typesafe.akka" % "akka-http-experimental_2.11" % "2.4.11",
  "org.scalatest" % "scalatest_2.11" % "3.0.0",
  "org.scalactic" % "scalactic_2.11" % "3.0.0"
)

dependencyOverrides += "org.scala-lang" % "scala-compiler" % scalaVersion.value