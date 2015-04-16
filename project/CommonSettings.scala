import sbt._
import sbt.Keys._

object CommonSettings {
  lazy val settings = Seq(
    organization := "feh.tec",
    scalaVersion := "2.11.6",
    resolvers           += "Fehu's github repo" at "http://fehu.github.io/repo",
    libraryDependencies += "nu.pattern" % "opencv" % "2.4.9-7"
  )
}
