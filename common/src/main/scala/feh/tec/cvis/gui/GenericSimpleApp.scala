package feh.tec.cvis.gui

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import feh.tec.cvis.gui.FileDropped._
import feh.util._
import feh.util.file._

import scala.swing.Swing._
import scala.swing._

trait GenericSimpleApp extends App{
  type AppFrame <: GenericSimpleAppFrame

  var frames: Set[AppFrame]
}

/** An extendable simple swing frame app
 *
 *  Has 3 panes:
 *              - original preview
 *              - result preview
 *              - configuration
 *
 *  Supports file drop
 */
trait GenericSimpleAppFrame extends Frame{
  type Preview <: Component
  type Config  <: Panel

  val original: Preview
  val modified: Preview
  val configurations: Config

  protected def filesDropped(files: List[File])

  protected def defaultSize: Dimension
}

object GenericSimpleAppFrame{
  
  trait SimplePreview extends ScrollPane{

    def img: BufferedImage  //val img = ImageIO.read(imageSrc)

    horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded

    private def w = img.getWidth
    private def h = img.getHeight

    val underlying = new Component {
      maximumSize = w -> h
      minimumSize = w -> h
      preferredSize = w -> h

//      new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
      override def paint(g: Graphics2D): Unit = g.drawImage(img, 0, 0, null)
    }

    this.contents = underlying
  }


  trait NewFrameOnFileDrop{
    frame: Frame =>

    def newFrame(file: File)

    frame.peer.getRootPane onFilesDropped filesDropped

    protected def filesDropped(files: List[File]): Unit = files foreach newFrame
  }

  class EmptyFrame(emptyText: String,
                   frameTitle: String,
                   emptySize: Dimension,
                   defaultSize: Dimension,
                   regNewFrame: SimpleFrame => Unit,
                   unregAFrame: SimpleFrame => Unit,
                   onEmptyClose: => Unit)
    extends Frame with NewFrameOnFileDrop
  {
    title = frameTitle
    contents = new Label(emptyText)

    minimumSize   = emptySize
    preferredSize = emptySize

    def newFrame(file: File): Unit = SimpleFrame(file, frameTitle, defaultSize, regNewFrame, unregAFrame) |> {
      fr => regNewFrame(fr)
            this.close()
            fr.open()
    }

    override def closeOperation(): Unit = {
      onEmptyClose
      super.closeOperation()
    }
  }
  
  class SimpleFrame(val image: BufferedImage,
                    frameTitle: String,
                    protected val defaultSize: Dimension,
                    protected val regNewFrame: SimpleFrame => Unit,
                    protected val unregAFrame: SimpleFrame => Unit
                     )
    extends GenericSimpleAppFrame
    with NewFrameOnFileDrop
  {
    type Preview = SimplePreview
    type Config = Panel

    val original: Preview = new SimplePreview{ def img = image }
    val modified: Preview = new SimplePreview{ def img = image }  // todo
    val configurations: Config = new Panel{ _contents += new Label("config") }

    this.title = frameTitle

    this.contents = new SplitPane(Orientation.Vertical, 
                                  new GridPanel(2, 1){_contents ++= Seq(modified, original)},
                                  configurations
    )


    override def closeOperation() = {
      super.closeOperation()
      unregAFrame(this)
    }

    def newFrame(file: File): Unit = SimpleFrame(file, frameTitle, defaultSize, regNewFrame, unregAFrame) |> {
      fr => regNewFrame(fr)
            fr.open()
    }
  }

  object SimpleFrame{
    def apply(image: BufferedImage,
              frameTitle: String,
              defaultSize: Dimension,
              regNewFrame: SimpleFrame => Unit,
              unregAFrame: SimpleFrame => Unit): SimpleFrame =
      new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
    
    def apply(file: File,
              frameTitle: String,
              defaultSize: Dimension,
              regNewFrame: SimpleFrame => Unit,
              unregAFrame: SimpleFrame => Unit): SimpleFrame =
    {
      val img = file.withInputStream(ImageIO.read).get
      new SimpleFrame(img, frameTitle, defaultSize, regNewFrame, unregAFrame)
    }
  }

  class DefaultApp( val appTitle: String,
                    emptySize: Dimension,
                    defaultSize: Dimension,
                    emptyText: String = "Drop some files here") extends GenericSimpleApp{
    type AppFrame = SimpleFrame
    var frames: Set[AppFrame] = Set()
    
    protected lazy val emptyFrame: EmptyFrame = new EmptyFrame(emptyText, appTitle, emptySize, defaultSize,
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