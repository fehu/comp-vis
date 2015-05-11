package feh.tec.cvis.gui

import java.awt.color.ColorSpace
import java.awt.image._
import java.awt.{Color, Dimension, Transparency}
import java.io.File
import java.util
import javax.imageio.ImageIO
import javax.swing.filechooser.FileNameExtensionFilter

import feh.dsl.swing2.{Var, Monitor, Control}
import feh.dsl.swing2.ComponentExt._
import feh.tec.cvis.common.cv.BufferedImageColor
import feh.tec.cvis.common.cv.describe.{CallHistoryContainer, CallDescriptor, CallHistory}
import feh.tec.cvis.gui.FileDrop._
import feh.util._
import feh.util.file._
import org.opencv.core.{CvType, Mat}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.swing._
import scala.swing.event.{MouseClicked, SelectionChanged}
import scala.util.Failure

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
    var LayoutDebug = false

    def setDebugBorder[B <: AbstractDSLBuilder](color: Color)(b: B): B =
      if(LayoutDebug) b.affect(_.border = Swing.LineBorder(color)).asInstanceOf[B]
      else            b

    def setDebugBorder(c: Component, color: Color): Unit = if(LayoutDebug) c.border = Swing.LineBorder(color)

    type Preview = SimplePreview

//    val original: Preview = new SimplePreview{ def img = originalImage }
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

    protected val UpperPanel: Panel

    lazy val scrollableTabs = scrollable()(
      tabs(_.Top, _.Scroll)(configurations map (_.swap: LayoutElem)),
      "scrollable-tabs"
    ) |> setDebugBorder(Color.green)

    val layout: List[AbstractLayoutSetting] = split(_.Vertical)(
//      panel.grid(2, 1)(original -> "image-original", modified -> "image-modified") -> "left-panel",
      modified -> "image-modified",
      panel.gridBag(
        place(scrollableTabs).transform(_.addLayout(_.anchor = Anchor.North,
                                                    _.weighty = 1,
                                                    _.weightx = 1,
                                                    _.fill = Fill.Both
                                                  ))                                  at theCenter,
        place(UpperPanel, "apply")
        .transform(_.addLayout(
          _.weighty = 0,
          _.fill = Fill.Horizontal
          ))                                                                          at theNorth
      ) -> "right-panel"
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

    /** sets the affected image's color type */
    private var _imageColorType = BufferedImageColor.mode(originalImage)
    def imageColorType = _imageColorType

    /** sets the affected image */
    private var _imageMat = originalMat
    
    protected def imageMat = _imageMat
    protected def affectImageMat(f: Mat => Unit) = {
      f(imageMat)
      _modifiedImage = toBufferImage(imageMat)
    }
    
    protected def setImageMat(mat: Mat) = {
      _imageMat = mat
      updateModifiedImage()
    }

    private var _modifiedImage = toBufferImage(imageMat)
    def modifiedImage = _modifiedImage
    def updateModifiedImage() = _modifiedImage = toBufferImage(imageMat)

    case class Runner[Params, Src, R](exec: Runner.OnEachStep => Params => Src => R)
    case class AtomicRunner[Params, Src, R](exec: Params => Src => R)
    case class RecursiveRunner[T, Params, Src, R](exec: Params => Src => T => Either[T, R]){
      def runner(initial: T, maxTries: Int, err: String) = Runner[Params, Src, R] {
        nextStep => params => src =>
          val ex = exec(params)(src)
          doUntil(initial, maxTries) { t => nextStep(); ex(t) }
          .right
          .getOrElse(sys.error(err))
      }

    }

    object Runner{
      implicit def atomicRunnerIsRunner[Params, Src, R](a: AtomicRunner[Params, Src, R]): Runner[Params, Src, R] = Runner(_ => a.exec)

      type OnEachStep = () => Unit
      
      def atomic[Params, Src, R](exec: Src => Params => R): AtomicRunner[Params, Src, R] = new AtomicRunner(params => src => exec(src)(params))
    }


    // Mat support
    def toMat(img: BufferedImage): Mat
    def toBufferImage(mat: Mat, tpeOpt: Option[Int] = None): BufferedImage

    
    trait PanelExec[Src, R] extends Component{
      conf: GenericConfigurationPanel =>

      type Params

      def steps: Int

      def classTag: ClassTag[R]
      def runner: Runner[Params, Src, R]

      protected def throwIfInterrupted()
    }

    trait PanelExecSimple[Src, R] extends PanelExec[Src, R]{
      conf: GenericConfigurationPanel =>

      def getParams(): Params
      def getSrc: Src
      def setResult: R => Unit

      def exec(): R = runner.exec(throwIfInterrupted)(getParams())(getSrc)

      protected def throwIfInterrupted()
    }

    trait MatPanelExec extends PanelExec[Mat, Mat]{
      conf: GenericConfigurationPanel =>

      def classTag =  scala.reflect.classTag[Mat]
    }


    def panelExec[R](mp: PanelExec[_, R]) = mp match {
      case mps: PanelExecSimple[_, R] => mps.setResult(mps.exec())
    }
    
    def repaintImage() = frame.modified.repaint()
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

    def toBufferImage(mat: Mat, tpeOpt: Option[Int] = None): BufferedImage = {
      val tpe = bufferedImageTypeByMat(mat)

      val gray  = mat.channels() == 1
      val norm  = mat.channels() == 3
      val alpha = mat.channels() == 4
      val setFloat = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferFloat].getData)

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
                 val img = new BufferedImage(mat.width(), mat.height(), tpeOpt getOrElse tpe)
                 setData(img.getRaster.getDataBuffer)
                 img
             }
        .getOrElse(manually)
    }

    def bufferedImageTypeByMat(mat: Mat) = {
      val gray  = mat.channels() == 1
      val norm  = mat.channels() == 3
      val alpha = mat.channels() == 4

      val setByte  = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferByte].getData)
      val setShort = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferShort].getData)
      val setInt   = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferInt].getData)
      val setFloat = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferFloat].getData)


      PartialFunction.condOpt(mat.depth()) {
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
    }
  }

  trait HistorySupport extends FrameExec{
    frame: GenericSimpleAppFrame =>

    trait PanelExecHistory[Src, R] extends PanelExec[Src, R]{
      conf: GenericConfigurationPanel =>

      type Params = CallHistory.Entry[_]

      def getSrc: CallHistoryContainer[Src]
      def setResult: CallHistoryContainer[R] => Unit

      def callDescriptor: CallDescriptor[R]
      def params: Set[CallHistory.ArgEntry[_]]

      protected def historyEntry  = CallHistory.Entry(callDescriptor, params)

      def exec(): CallHistoryContainer[R] = getSrc.affect(historyEntry)(runner.exec(throwIfInterrupted)(historyEntry))
    }


    override def panelExec[R](mp: PanelExec[_, R]): Unit = mp match {
      case peh: PanelExecHistory[_, _]  => hPanelExec(peh)
      case pe                           => super.panelExec(pe)
    }

    def hPanelExec[R](mp: PanelExecHistory[_, R]) { mp.setResult(mp.exec()) }


  }

  trait ConfigurationsPanelBuilder{
    frame: GenericSimpleAppFrame with FrameExec with LayoutDSL =>

    def setDebugBorder[B <: AbstractDSLBuilder](color: Color)(b: B): B
    def setDebugBorder(c: Component, color: Color): Unit

    trait SimpleVerticalPanel extends FlowPanel with GenericConfigurationPanel{
      pexec: PanelExec[_, _] =>

      setDebugBorder(this, Color.red)

      val elems: Seq[(String, Seq[Component])]

      def updateForms(): Unit = elems.foreach(_._2.foreach(_.tryUpdate()))

      contents += thePanel

      protected lazy val thePanel = panel
        .box(_.Vertical)(prepareElems: _*)
        .pipe(setDebugBorder(Color.magenta))
        .component

      private def prepareElems = elems.map{ case (k, v) => mkSmallPanel(k)(v) -> k }.toSeq

      protected def mkSmallPanel(id: String)(seq: Seq[Component]) =
        panel
          .box(_.Vertical)(seq.zip(Range(0, seq.length)).map(p => p._1 -> (id + "-" + p._2)): _*)
          .pipe(setDebugBorder(Color.blue))

    }

    lazy val tabs = componentAccess.get("scrollable-tabs").get.asInstanceOf[ScrollPane].contents.head.asInstanceOf[TabbedPane]

    def currentExec = _currentConfig

    object UpperPanel extends GridBagPanel with GenericConfigurationPanel with UpdateInterface{
      def updateForms() = {}
      def updateForm()  = {}

      private var locked = false

      override def lockForm(): Unit = {
        applyButton.text = "Abort"
        progressBar.variable.set(0)
        progressBar.component.max = _currentConfig.steps
        progressBar.component.unlock()
        repaintCheckbox.component.lock()
        locked = true
      }
      override def unlockForm(): Unit = {
        applyButton.text = "Apply"
        progressBar.variable.set(0)
        progressBar.component.label = null
        progressBar.component.lock()
        repaintCheckbox.component.unlock()
        locked = false
      }

      layout ++= Seq(
        repaintCheckbox.component -> (new Constraints() $$ (_.grid = 0 -> 0))
      , progressBar.component     -> (new Constraints() $$ (_.grid = 1 -> 0) $$ (_.weightx = 1) $$ (_.fill = Fill.Horizontal))
      , applyButton.component     -> (new Constraints() $$ (_.grid = 0 -> 1) $$ (_.weightx = 1) $$ (_.fill = Fill.Horizontal) $$ (_.gridwidth = 2))
      )

      def applyAction() = {
        lockForm()
        tabs.tryLock()

        Future{ panelExec(currentExec) }.onComplete{
         res =>
           tabs.tryUnlock()
           tabs.tryUpdate()
           unlockForm()
           finished()

           res match {
             case Failure(Interrupted) =>
             case Failure(thr) => failure(thr)
             case _ =>
           }
       }
      }

      var onError: List[Throwable => Unit] = Nil

      def abortAction() = interrupt()


      lazy val applyButton: Button = triggerFor{
        if(locked) abortAction() else applyAction()
      }.button("Apply").formMeta.form


      lazy val progressBar = Monitor(currentProgress, new ProgressBar $$ (_.labelPainted = true))

      lazy val repaintCheckbox = Control(repaint_?, new CheckBox("repaint"))

      def updateProgress(msg: String) = {
        progressBar.variable.affect(_ + 1)
        progressBar.component.label = msg
      }
    }

    def failure(thr: Throwable) = {
//      thr.printStackTrace()
      UpperPanel.onError.foreach(_(thr))
      Dialog.showMessage(parent = frame,
                         message = thr.toString,
                         title = "Error",
                         messageType = Dialog.Message.Error)
      throw thr
    }

    object failure{
      def pf: PartialFunction[Throwable, Nothing] = { case th: Throwable => failure(th) }
    }

    private var _currentConfig: PanelExec[_, _] =  configurations.head._2
    protected lazy val currentProgress = Var(0)
    protected lazy val repaint_? = Var(true)

    private var   _interrupted = false
    def           interrupted_? = synchronized(_interrupted)
    protected def finished() = synchronized(_interrupted = false)
    def           interrupt() = synchronized(_interrupted = true)

    case object Interrupted extends Exception


    tabs |> {
      tp =>
        frame.listenTo(tp.selection)
        frame.reactions += {
          case SelectionChanged(`tp`) =>
            val sel = tp.selection.index match {
              case -1 => None
              case x  => Some(x)
            }
            sel.map(tp.pages).foreach(page => _currentConfig = configurations.find(_._1 == page.title).get._2)
        }
    }

  }
}
