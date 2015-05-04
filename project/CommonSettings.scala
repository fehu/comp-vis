import java.io.File
import sbt._
import sbt.Keys._

object CommonSettings {
  lazy val settings = Seq(
    organization := "feh.tec"
  , scalaVersion := "2.11.6"
  , resolvers           += "Fehu's github repo" at "http://fehu.github.io/repo"
  , unmanagedBase in Compile := file("libs")
  )

  System.setProperty("java.library.path", sys.props("java.library.path")
                        + File.pathSeparator + Seq(sys.props("user.dir"), "libs", "opencv", systemDependantPath).mkString(File.separator))

  def systemDependantPath = (sys.props("os.name").toLowerCase, sys.props("os.arch")) match {
    case (os, "amd64" | "x86_64") if os startsWith "linux"    => "linux-x64"
    case (os, "x86"   | "i386")   if os startsWith "linux"    => "linux-x86"
    case (os, "amd64" | "x86_64") if os startsWith "windows"  => "win-x64"
    case (os, "x86"   | "i386")   if os startsWith "windows"  => "win-x86"
    case (os, "amd64" | "x86_64") if os startsWith "mac os x" => "osx-x64"
    case _ =>  sys.error("unsupported os and arch")
  }

  val supported = "linux-x64" :: "linux-x86" ::
                  "win-x64"   :: "win-x86"   ::
                  "osx-x64"   :: Nil
}
