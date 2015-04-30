package feh.tec.cvis.common

import org.opencv.core.Core


object CV {
  private var loaded = false
  
  def loadNative() = if(!loaded) try {
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
    loaded = true
  } 
}
