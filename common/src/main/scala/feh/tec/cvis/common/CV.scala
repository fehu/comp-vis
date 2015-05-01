package feh.tec.cvis.common

import feh.util.file._

object CV {
  private var loaded = false
  
  def loadNative() = if(!loaded) try {
    val lib = extractLib()
    Runtime.getRuntime.load(lib.getAbsolutePath)
    loaded = true
  }

  lazy val cvLib = "libopencv_java300"

  def extractLib(): File = {
    val (path, suff) = systemDependant
    val lib = ClassLoader.getSystemResourceAsStream("opencv" / path / cvLib + suff)
              .ensuring(_ != null, "no lib found in resources")
    val tmp = File.temporaryDir("opencv").createFile(s"$cvLib." + suff).get
    tmp.withOutputStream(File.write(lib)).get
    tmp
  }

  def systemDependant = (sys.props("os.name").toLowerCase, sys.props("os.arch")) match {
    case (os, "amd64" | "x86_64") if os startsWith "linux"    => "linux-x64"  -> "so"
    case (os, "x86"   | "i386")   if os startsWith "linux"    => "linux-x86"  -> "so"
    case (os, "amd64" | "x86_64") if os startsWith "windows"  => "win-x64"    -> "dll"
    case (os, "x86"   | "i386")   if os startsWith "windows"  => "win-x86"    -> "dll"
    case (os, "amd64" | "x86_64") if os startsWith "mac os x" => "osx-x64"    -> "dylib"
  }
}
