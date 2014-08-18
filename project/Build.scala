package racewatcher

import sbt._
import Keys._

object Ddaq extends Build {

  val appVersion = "0.1"
  val scala = "2.11.2"

  val jodas = Seq("joda-time" % "joda-time" % "2.4", "org.joda" % "joda-convert" % "1.7")
  val shapeless = "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full

  val ddaqDeps = Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "org.specs2" %% "specs2" % "2.4" % "test",
    "com.chuusai" %% "shapeless" % "2.0.0"
  ) ++ jodas

  val commonResolvers = Seq(
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "releases"  at "http://oss.sonatype.org/content/repositories/releases",
    "spray repo" at "http://repo.spray.io",
    Resolver.url("sbt-plugin-releases", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
    "Pellucid Bintray"  at "http://dl.bintray.com/content/pellucid/maven",
    "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
    "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
  )

  def project(name: String, path: String) = sbt.Project(
    name,
    base = file(path),
    settings = Defaults.defaultSettings ++ Seq(
      scalaVersion := scala,
      resolvers ++= commonResolvers,
      version := appVersion,
      libraryDependencies := ddaqDeps
    )
  )

  val scunits = project("scunits", ".") 

  override def rootProject = Some(scunits)
}