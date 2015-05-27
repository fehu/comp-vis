package feh.tec.cvis.gui

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.io.File
import feh.dsl.swing.{SwingFrameAppCreation, AbstractGUI}
import feh.util._
import feh.util.file._

import scala.swing._

trait GenericSimpleApp extends App with AbstractGUI with GenericConfigurationGUI{
  type AppFrame <: GenericSimpleAppFrame

  var frames: Set[AppFrame]


  /** An extendable simple swing frame app
    *
    *  Has 3 panes:
    *              - original preview
    *              - result preview
    *              - configuration
    *
    *  Supports file drop
    */
  trait GenericSimpleAppFrame extends GuiFrame with GenericGUIFrame{
    type Preview <: Component
    type Config  <: GenericConfigurationPanel

    val originalImage: BufferedImage
    def modifiedImage: BufferedImage
    
//    val original: Preview
    val modified: Preview
    val configurations: Seq[(String, Config)]
    

    protected def filesAdded(files: List[File])

    protected def defaultSize: Dimension
  }

}

object GenericSimpleApp{

  abstract class DefaultApp( val appTitle: String,
                    emptySize: Dimension,
                    defaultSize: Dimension,
                    emptyText: String = "Click or Drop some files here") extends GenericSimpleAppFrameImplementation{
    type AppFrame = SimpleFrame
    var frames: Set[AppFrame] = Set()

    protected lazy val emptyFrame: EmptyFrame = new EmptyFrame( emptyText, appTitle, emptySize, defaultSize,
                                                                regNewFrame = frames += _,
                                                                unregAFrame = {
                                                                  fr =>
                                                                    frames -= fr
                                                                    if(frames.isEmpty) allFramesClosed()
                                                                },
                                                                onEmptyClose = if(frames.isEmpty) sys.exit()
    )

    protected def allFramesClosed() = Dialog.showConfirmation(message = "All windows closed, close the app entirely?",
                                                              title = "Closing",
                                                              optionType = Dialog.Options.YesNo,
                                                              messageType = Dialog.Message.Question
                                                              ) |> {
      case Dialog.Result.Yes                       => sys.exit()
      case Dialog.Result.No | Dialog.Result.Closed => emptyFrame.open()
    }

    emptyFrame.open()
  }
}