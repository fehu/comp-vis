import java.io.File

import sbt._
import sbt.Keys._

object CommonSettings {
  lazy val settings = Seq(
    organization := "feh.tec",
    scalaVersion := "2.11.6",
    resolvers           += "Fehu's github repo" at "http://fehu.github.io/repo",
    unmanagedBase in Compile := file("libs_opencv")
  )

  System.setProperty("java.library.path", "-Djava.library.path=" + sys.props("java.library.path") + File.pathSeparator
                                                                 + Seq("libs_opencv", "linux-x64").mkString(File.separator))


}
