package feh.tec.cvis.common

import feh.util.Path
import feh.util.file._
import org.opencv.core.Core
import scala.collection.convert.decorateAsScala._

object CV {
  private var loaded = false

  lazy val cvLib = "libopencv_java300"

  lazy val resourceDir = "opencv/" + systemDependant._1
  
  def loadNative() = {
    val loadedLibs = listLibs.map(Path(_, File.separatorChar))

    if (!loaded)
      if (loadedLibs.exists(p => p.path.head.startsWith(Core.NATIVE_LIBRARY_NAME) || p.path.head.startsWith(cvLib))) loaded = true
      else
        try {
          val libPath = sys.props("java.library.path").split(File.pathSeparatorChar).toList
          val (_, suff) = systemDependant

          if (libPath.exists(_.contains(resourceDir))) System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
          else Runtime.getRuntime.load(extractLib().getAbsolutePath)

          loaded = true
        }
        catch {
          case thr: Throwable =>
            thr.printStackTrace()
            sys.exit(1)
        }
  }

  def extractLib(): File = {
    val (path, suff) = systemDependant
    val lib = ClassLoader.getSystemResourceAsStream(resourceDir + s"/$cvLib.$suff").ensuring(_ != null, "no lib found in resources")
    val tmp = File.temporary(s"$cvLib.$suff")
    tmp.withOutputStream(File.write(lib)).get
    tmp
  }

  lazy val systemDependant = (sys.props("os.name").toLowerCase, sys.props("os.arch")) match {
    case (os, "amd64" | "x86_64") if os startsWith "linux"    => "linux-x64"  -> "so"
    case (os, "x86"   | "i386")   if os startsWith "linux"    => "linux-x86"  -> "so"
    case (os, "amd64" | "x86_64") if os startsWith "windows"  => "win-x64"    -> "dll"
    case (os, "x86"   | "i386")   if os startsWith "windows"  => "win-x86"    -> "dll"
    case (os, "amd64" | "x86_64") if os startsWith "mac os x" => "osx-x64"    -> "dylib"
  }

  def listLibs = {
    val f = classOf[ClassLoader].getDeclaredField("loadedLibraryNames")
    f.setAccessible(true)
    f.get(getClass.getClassLoader).asInstanceOf[java.util.List[String]].asScala
  }
}
