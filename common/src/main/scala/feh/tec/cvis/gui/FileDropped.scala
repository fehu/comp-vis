package feh.tec.cvis.gui

import java.io.File

import mdesl.test.FileDrop
import mdesl.test.FileDrop.Listener

import scala.swing.Component

/**
 * wrappings for [[mdesl.test.FileDrop]]
 */
object FileDropped {
  
  protected def _filesDropped(c: java.awt.Component, f: List[File] => Unit) = new FileDrop(c,
    new Listener {
      def filesDropped(files: Array[File]): Unit = f(files.toList)
    }
  )
  
  implicit class FileDropCreationWrapper(c: Component){
    def onFilesDropped(f: List[File] => Unit) = _filesDropped(c.peer, f)
  }

  implicit class FileDropCreationJWrapper(c: java.awt.Component){
    def onFilesDropped(f: List[File] => Unit) = _filesDropped(c, f)
  }

}
