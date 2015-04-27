package feh.tec.cvis.gui

import javax.swing.filechooser.{FileNameExtensionFilter, FileFilter}

import scala.collection.mutable
import java.awt.{Color, Dimension}
import java.awt.image.{DataBuffer, DataBufferInt, DataBufferByte, BufferedImage}
import java.io.File
import javax.imageio.ImageIO
import feh.tec.cvis.common.MatCreation
import feh.tec.cvis.gui.Helper._
import org.opencv.core.{CvType, Mat}
import scala.reflect.ClassTag
import scala.swing._
import feh.util._
import feh.util.file._
import FileDrop._
import scala.swing.event.{Key, MouseClicked}

trait GenericSimpleAppFrameImplementation extends GenericSimpleApp{

  trait NewFrameOnFileDrop{
    frame: Frame =>

    def newFrame(file: File)

    frame.peer.getRootPane onFilesDropped (borderOnDrag, filesAdded)

    protected lazy val borderOnDrag = Swing.MatteBorder(10, 10, 10, 10, Color.blue)

    protected def filesAdded(files: List[File]): Unit = files foreach newFrame
  }

  class EmptyFrame(emptyText: String,
                   frameTitle: String,
                   emptySize: Dimension,
                   defaultSize: Dimension,
                   regNewFrame: SimpleFrame => Unit,
                   unregAFrame: SimpleFrame => Unit,
                   onEmptyClose: => Unit
                    )
    extends Frame with NewFrameOnFileDrop
  {
    frame =>

    title = frameTitle
    contents = label

    minimumSize   = emptySize
    preferredSize = emptySize

    def newFrame(file: File): Unit = mkSimpleFrame(file, frameTitle, defaultSize, regNewFrame, unregAFrame) |> {
      fr => regNewFrame(fr)
        this.close()
        fr.start()
    }

    override def closeOperation(): Unit = {
      onEmptyClose
      super.closeOperation()
    }

    lazy val label = new Label(emptyText)

    listenTo(label.mouse.clicks)
    reactions += {
      case ev: MouseClicked /*(_, _, _ /*??*/, _, _)*/ =>
        println("AA")
        new FileChooser(new File(sys.props("user.home"))) $$ {
        fch =>
          fch.fileFilter = new FileNameExtensionFilter("images", "jpg", "jpeg", "png", "gif")
          fch.multiSelectionEnabled = true
          fch.title = "Select files"
          fch.showDialog(frame, "Select") match {
            case FileChooser.Result.Approve => filesAdded _ $ fch.selectedFiles.toList
            case FileChooser.Result.Cancel  => // do nothing
            case FileChooser.Result.Error   => Dialog.showMessage(parent = frame,
                                                                  message = "Failed to select the file",
                                                                  title = "Error",
                                                                  Dialog.Message.Error)
          }
      }
    }
  }

  abstract class SimpleFrame( val originalImage: BufferedImage,
                                  frameTitle: String,
                    protected val defaultSize: Dimension,
                    protected val regNewFrame: SimpleFrame => Unit,
                    protected val unregAFrame: SimpleFrame => Unit
                     )
    extends GenericSimpleAppFrame
    with NewFrameOnFileDrop
    with Frame9PositionsLayoutBuilder
    with FrameExec
  {
    type Preview = SimplePreview

    val original: Preview = new SimplePreview{ def img = originalImage }
    val modified: Preview = new SimplePreview{ def img = modifiedImage }

    this.title = frameTitle

    def newFrame(file: File): Unit = mkSimpleFrame(file, frameTitle, defaultSize, regNewFrame, unregAFrame) |> {
      fr => regNewFrame(fr)
        fr.start()
    }

    def start(): Unit = {
      buildLayout()
      open()
    }
    def stop(): Unit = unregAFrame(this)

    val layout: List[AbstractLayoutSetting] = split(_.Vertical)(
      panel.grid(2, 1)(original -> "image-original", modified -> "image-modified") -> "left-panel",
      (configurations $$ {_.minimumSize = 200 -> 200}) -> "configurations"
    )
  }

  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit  ): SimpleFrame

  def mkSimpleFrame(file: File,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit  ): SimpleFrame = {
    val img = file.withInputStream(ImageIO.read).get
    mkSimpleFrame(img, frameTitle, defaultSize, regNewFrame, unregAFrame)
  }

//  def mkSimpleFrame(params: MkSimpleFrameParams): SimpleFrame = params.image match {
//    case Left(img)   => mkSimpleFrame(img,  params.frameTitle, params.defaultSize, params.regNewFrame, params.unregAFrame)
//    case Right(file) => mkSimpleFrame(file, params.frameTitle, params.defaultSize, params.regNewFrame, params.unregAFrame)
//  }
//
//
//  case class MkSimpleFrameParams(image: Either[BufferedImage, File],
//                                 frameTitle: String,
//                                 defaultSize: Dimension,
//                                 regNewFrame: SimpleFrame => Unit,
//                                 unregAFrame: SimpleFrame => Unit)

  trait FrameExec{
    frame: GenericSimpleAppFrame =>

    type Config  <: GenericConfigurationPanel with PanelExec[_, _]

    protected var imageMat = toMat(originalImage)

    def modifiedImage = toBufferImage(imageMat)
    
    case class Runner[Params, Src, R](exec: Params => Src => R)
    object Runner{
      def create[Params, Src, R](exec: Src => Params => R): Runner[Params, Src, R] = new Runner(params => src => exec(src)(params))
    }

//    case class RunnerExt[T, R](get: T => Runner[R]){
//      def runner: T =>  = t => Runner(exec(t))
//    }
//    object RunnerExt{
//      def apply[R](runner: Runner[R]): RunnerExt[T, R]
//    }
    
    case class TransformsFor[Src](get: Seq[Runner[_, Src, _]])
    
//    def exec[R](panel: PanelExec[R])
    
    // tags todo used ??
//    def underlyingTag(buffi: BufferedImage): ClassTag[_]
//    def underlyingTag(mat: Mat): ClassTag[_]

    // Mat support
    def toMat(img: BufferedImage): Mat
    def toBufferImage(mat: Mat): BufferedImage

    
    trait PanelExec[Src, R]{
      conf: GenericConfigurationPanel =>

      type Params

      def classTag: ClassTag[R]
      def runner: Runner[Params, Src, R]
      
      def exec(): R
    }

    trait MatPanelExec extends PanelExec[Mat, Mat]{
      conf: GenericConfigurationPanel =>


      def classTag =  scala.reflect.classTag[Mat]

      protected def getParams(): Params
      
      def exec(): Mat = runner.exec(getParams())(frame.imageMat)
    }


    def matPanelExec(mp: MatPanelExec) = {
      val mat = mp.exec()
      frame.imageMat = mat
      frame.modified.repaint()
    }
  }

  trait ConfigurationsPanelBuilder{
    frame: GenericSimpleAppFrame with FrameExec =>

    trait SimpleVerticalPanel extends GridBagPanel with GenericConfigurationPanel{
      self: MatPanelExec =>

      val elems: Map[String, Seq[Component with UpdateInterface]]

      lazy val applyButton = triggerFor{
        try imageMat = exec()
        catch {
          case thr: Throwable => Dialog.showMessage(message = thr.toString,
                                                    title = "Error",
                                                    messageType = Dialog.Message.Error)
        }
      }.button("Apply")

      def updateForms(): Unit = elems.foreach(_._2.foreach(_.updateForm()))

      protected lazy val thePanel = panel.grid(2, 1)(
        panel.grid(elems.size, 1)(prepareElems: _*) -> noId,
        applyButton -> "apply"
      )
//        .box(_.Vertical)(prepareElems: _*)
//        .doNotGlue

      layout += thePanel.component -> (new Constraints()  $$ { _.fill = GridBagPanel.Fill.Horizontal }
                                                          $$ { _.weightx = 1 }
                                                          $$ { _.weighty = 1 }
                                                          $$ { _.anchor = GridBagPanel.Anchor.NorthEast }
                                                          )

      private def prepareElems = elems.map{ case (k, v) => mkSmallPanel(k)(v) -> k }.toSeq

      private def mkSmallPanel(id: String)(seq: Seq[Component with UpdateInterface]) =
        panel.grid(seq.length, 1)(seq.zip(Range(0, seq.length)).map(p => p._1 -> (id + "-" + p._2)): _*)
//          .affect(_.border = Swing.LineBorder(Color.red))

    }

  }
}
