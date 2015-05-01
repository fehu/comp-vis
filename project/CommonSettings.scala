import java.io.File
import sbt._
import sbt.Keys._

object CommonSettings {
  lazy val settings = Seq(
    organization := "feh.tec",
    scalaVersion := "2.11.6",
    resolvers           += "Fehu's github repo" at "http://fehu.github.io/repo",
    unmanagedBase in Compile := file("libs/opencv")
  )

  System.setProperty("java.library.path",
                     "-Djava.library.path=" + sys.props("java.library.path") + File.pathSeparator
                                            + Seq("libs", "opencv", systemDependantPath).mkString(File.separator))

  def systemDependantPath = (sys.props("os.name").toLowerCase, sys.props("os.arch")) match {
    case (os, "amd64" | "x86_64") if os startsWith "linux"    => "linux-x64"
    case (os, "x86"   | "i386")   if os startsWith "linux"    => "linux-x86"
    case (os, "amd64" | "x86_64") if os startsWith "windows"  => "win-64"
    case (os, "x86"   | "i386")   if os startsWith "windows"  => "win-32"
    case (os, "amd64" | "x86_64") if os startsWith "mac os x" => "osx-64"
    case _ =>  sys.error("unsupported os and arch")
  }
}
