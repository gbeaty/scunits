package racewatcher

import sbt._
import Keys._

object Ddaq extends Build {

  val appVersion = "0.0.1"
  val scala = "2.11.2"

  val testDeps = Seq(
    "org.specs2" %% "specs2" % "2.4" % "test"
  )

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

  def project(name: String) = sbt.Project(
    name,
    base = file(name),
    settings = Defaults.defaultSettings ++ Seq(      
      scalaVersion := scala,
      resolvers ++= commonResolvers,
      version := appVersion,
      organization := "org.scunits"
    )
  )

  val core = project("core").settings(
    // publishTo := Some("Sonatype Snapshots Nexus" at "https://oss.sonatype.org/content/repositories/snapshots")
  )
  val test = project("test").dependsOn(core).settings(
    libraryDependencies ++= testDeps
  )

  override def rootProject = Some(core)
}