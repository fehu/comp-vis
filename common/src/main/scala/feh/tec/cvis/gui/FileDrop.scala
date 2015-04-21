/* Taken from   https://github.com/mattdesl/lwjgl-basics/blob/master/test/mdesl/test/FileDrop.java
 *              master - cdf5c33c5365848e7e052036e1ff549dfc6c109f.
 * Rewritten to Scala for Java 8 by fehu.
 */

package feh.tec.cvis.gui

import java.awt.Container
import java.awt.datatransfer.{Transferable, UnsupportedFlavorException, DataFlavor}
import java.awt.dnd._
import java.awt.event.{HierarchyEvent, HierarchyListener}
import java.io.{BufferedReader, File}
import java.net.URI
import javax.swing.JComponent
import javax.swing.border.Border
import scala.collection.convert.decorateAsScala._
import feh.util._

import scala.swing.{Component, Dialog}

trait FileDrop {

  def filesDraggedIn(): Boolean
  def filesDraggedOut()
  def filesDropped: List[File] => Unit

  def component: Either[JComponent, Component]
  protected lazy val c: JComponent = component.right.map(_.peer).merge

  protected lazy val dropListener: DropTargetListener = new DropTargetListener{
    def dragOver(dtde: DropTargetDragEvent): Unit = {}

    def dragExit(dte: DropTargetEvent): Unit = filesDraggedOut()

    def drop(dtde: DropTargetDropEvent): Unit = {
      debugLog( "FileDrop: drop event." )

      val tr = dtde.getTransferable
      if(tr.isDataFlavorSupported(DataFlavor.javaFileListFlavor)){
        dtde.acceptDrop(java.awt.dnd.DnDConstants.ACTION_COPY)
        debugLog( "FileDrop: file list accepted." )

        try filesDropped(tr.getTransferData(DataFlavor.javaFileListFlavor)
                           .asInstanceOf[java.util.List[File]].asScala.toList
                        )
        catch {
          case ex: UnsupportedFlavorException => Dialog.showMessage(title = "File Drop Error",
                                                                    message = "Your system doesn't support file drop",
                                                                    messageType = Dialog.Message.Warning)
        }

      }
      filesDraggedOut()
      dtde.getDropTargetContext.dropComplete(true)
    }

    def dropActionChanged(dtde: DropTargetDragEvent): Unit = {
      debugLog("FileDrop: dragEnter event." )
      acceptOrReject(dtde, dtde.getTransferable)
    }

    def dragEnter(dtde: DropTargetDragEvent) = {
      debugLog("FileDrop: dragEnter event." )
      acceptOrReject(dtde, dtde.getTransferable)
    }
  }

  makeDropTarget(c, recursive = true)

  protected def acceptOrReject(dtde: DropTargetDragEvent, tr: Transferable) = {

    debugLog("isFileListDrop_? " + isFileListDrop_?(dtde))
    val fdi = filesDraggedIn()
    debugLog("filesDraggedIn" + fdi)
    debugLog("processRepresentationClassReader(dtde, tr)" + processRepresentationClassReader(dtde, tr))

    if (isFileListDrop_?(dtde) && fdi || processRepresentationClassReader(dtde, tr).nonEmpty && filesDraggedIn()) {
      dtde.acceptDrag(DnDConstants.ACTION_COPY)
      debugLog("FileDrop: event accepted.")
    }
    else {
      dtde.rejectDrag()
      debugLog("FileDrop: event rejected.")
    }
  }

  protected def isFileListDrop_?(ev: DropTargetDragEvent) = ev.getCurrentDataFlavors.toList match { // ev.getCurrentDataFlavors
    case Nil => debugLog("FileDrop: no data flavors."); false
    case flavors =>
      debugLog("Flavors: " + flavors)
      flavors.exists {
                       flavor => flavor equals DataFlavor.javaFileListFlavor //|| flavor.isRepresentationClassReader
                     }
  }

  protected def processRepresentationClassReader(dtde: DropTargetDragEvent, tr: Transferable) = dtde.getCurrentDataFlavors.collect{
    case flavor if flavor.isRepresentationClassReader =>
      val reader = new BufferedReader(flavor.getReaderForText(tr)) // causes some exceptions
      Y[List[File], List[File]](
        rec =>
          acc =>
            if(reader.ready())  new File(new URI(reader.readLine())) :: acc
            else                acc
      )(Nil)
  }.flatten

  protected def makeDropTarget(c: java.awt.Component, recursive: Boolean = true ): Unit  = {
    // Make drop target
    val dt = new DropTarget()

    dt.addDropTargetListener( dropListener )

    // Listen for hierarchy changes and remove the drop target when the parent gets cleared out.
    c.addHierarchyListener( new HierarchyListener {
      def hierarchyChanged(e: HierarchyEvent) = {
        debugLog( "FileDrop: Hierarchy changed." )

        Option(c.getParent).map(_ => new DropTarget(c, dropListener)) getOrElse c.setDropTarget(null)
      }
    })

    if (c.getParent != null) new DropTarget(c, dropListener)

    if (recursive) c match {
      case cc: Container => cc.getComponents foreach (makeDropTarget(_))
    }
  }

  var DEBUG = false
  protected def debugLog(msg: Any) = if (DEBUG) println(msg.toString)
}

object FileDrop{
  trait BorderOnDrag{
    self: FileDrop =>

    def filesInBorder: Border

    protected lazy val normalBorder = c.getBorder


    def filesDraggedIn() = { c.setBorder(filesInBorder); true }
    def filesDraggedOut() = c.setBorder(normalBorder)

    normalBorder // init lazy val
  }

  object BorderOnDrag{
    def apply(comp: Either[JComponent, Component], borderOnDrag: Border, dropped: List[File] => Unit) =
      new FileDrop with BorderOnDrag{
        def filesDropped  = dropped
        def component     = comp
        def filesInBorder = borderOnDrag
      }
  }


  implicit class FileDropCreationWrapper(c: Component){
    def onFilesDropped(borderOnDrag: Border, f: List[File] => Unit) = BorderOnDrag(Right(c), borderOnDrag, f)
  }

  implicit class FileDropCreationJWrapper(c: JComponent){
    def onFilesDropped(borderOnDrag: Border, f: List[File] => Unit) = BorderOnDrag(Left(c), borderOnDrag, f)
  }
}