package feh.tec.cvis.gui

import java.awt.color.ColorSpace
import java.util
import javax.swing.filechooser.{FileNameExtensionFilter, FileFilter}
import java.awt.{Transparency, Color, Dimension}
import java.awt.image._
import java.io.File
import javax.imageio.ImageIO
import feh.tec.cvis.common.describe.CallDescriptor.Callable
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

    protected val originalMat = toMat(originalImage)

    protected def applyMat: Mat

    private var _imageMat = originalMat
    protected def imageMat = _imageMat
    protected def setImageMat(mat: Mat) = {
      _imageMat = mat
      _modifiedImage = toBufferImage(imageMat)
    }

    private var _modifiedImage = toBufferImage(imageMat)
    def modifiedImage = _modifiedImage

    case class Runner[Params, Src, R](exec: Params => Src => R)
    object Runner{
      def callable[Params, Src, R](f: Params => Callable[Src, R]): Runner[Params, Src, R] = Runner(
        params =>
          src =>
            f(params).call(src)
      )
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
      
      def exec(): Mat = runner.exec(getParams())(frame.applyMat)
    }


    def matPanelExec(mp: MatPanelExec) = {
      val mat = mp.exec()
      frame.setImageMat(mat)
      frame.modified.repaint()
    }
  }

  trait MatSupport extends FrameExec {
    frame: GenericSimpleAppFrame =>

    def toMat(img: BufferedImage): Mat = {
      val buff = img.getRaster.getDataBuffer

      val depth = DataBuffer.getDataTypeSize(buff.getDataType) match {
        case 8  => CvType.CV_8U
        case 16 => CvType.CV_16U
        case 32 => CvType.CV_32F
      }

      val channels = buff.getSize / (img.getHeight * img.getWidth)

      val mat = new Mat(img.getHeight, img.getWidth, CvType.makeType(depth, channels))
      buff match {
        case b: DataBufferByte    => mat.put(0, 0, b.getData)
        case b: DataBufferShort   => mat.put(0, 0, b.getData)
        case b: DataBufferUShort  => mat.put(0, 0, b.getData)
        case b: DataBufferInt     => mat.put(0, 0, b.getData)
        case b: DataBufferFloat   => mat.put(0, 0, b.getData)
        case b: DataBufferDouble  => mat.put(0, 0, b.getData: _*)
      }
      mat
    }

    def toBufferImage(mat: Mat): BufferedImage = {
      val gray  = mat.channels() == 1
      val norm  = mat.channels() == 3
      val alpha = mat.channels() == 4

      val setByte  = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferByte].getData)
      val setShort = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferShort].getData)
      val setInt   = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferInt].getData)
      val setFloat = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferFloat].getData)


      def tpe = PartialFunction.condOpt(mat.depth()) {
        // Byte
        case CvType.CV_8U  if gray  => BufferedImage.TYPE_BYTE_GRAY  -> setByte
        case CvType.CV_8U  if norm  => BufferedImage.TYPE_3BYTE_BGR  -> setByte
        case CvType.CV_8U  if alpha => BufferedImage.TYPE_4BYTE_ABGR -> setByte
        // UShort
        case CvType.CV_16U if gray  => BufferedImage.TYPE_USHORT_GRAY -> setShort
        // Int
        case CvType.CV_32S if norm  => BufferedImage.TYPE_INT_RGB  -> setInt
        case CvType.CV_32S if alpha => BufferedImage.TYPE_INT_ARGB -> setInt
        // Float todo ?? treat as int ???
        case CvType.CV_32F if norm  => BufferedImage.TYPE_INT_RGB  -> setInt
        case CvType.CV_32F if alpha => BufferedImage.TYPE_INT_ARGB -> setInt
      }

//
//      val img = new BufferedImage(mat.width(), mat.height(), tpe)
//      setData(img.getRaster.getDataBuffer)
//      img

      def mkGrayModel(btpe: Int, depth: Int) = {
        val cs  = ColorSpace.getInstance(ColorSpace.CS_GRAY)
        val nBits: Array[Int] = Array(depth)
        new ComponentColorModel(cs, nBits, alpha, true, Transparency.OPAQUE, btpe)
      }

      def manually = {
        val (colorModel, setData) = mat.depth() match{
          case CvType.CV_32F if gray => mkGrayModel(DataBuffer.TYPE_FLOAT, 32) -> setFloat
        }
        val raster = colorModel.createCompatibleWritableRaster(mat.width, mat.height)
        setData(raster.getDataBuffer)
        new BufferedImage(colorModel, raster, false, new util.Hashtable())
      }



      tpe.map{
               case (tpe, setData) =>
                 val img = new BufferedImage(mat.width(), mat.height(), tpe)
                 setData(img.getRaster.getDataBuffer)
                 img
             }
        .getOrElse(manually)
    }
  }

  trait ConfigurationsPanelBuilder{
    frame: GenericSimpleAppFrame with FrameExec =>

    trait SimpleVerticalPanel extends GridBagPanel with GenericConfigurationPanel{
      pexec: MatPanelExec =>

      val elems: Seq[(String, Seq[Component with UpdateInterface])]

      lazy val applyButton = triggerFor{
//        exec()
        try {
          matPanelExec(pexec)
          frame.updateForms()
        }
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
