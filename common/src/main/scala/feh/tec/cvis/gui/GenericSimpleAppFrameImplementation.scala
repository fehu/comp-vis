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

  abstract class SimpleFrame( val image: BufferedImage,
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

    val original: Preview = new SimplePreview{ def img = image }
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
      panel.gridBag(
        place(configurations $$ {_.minimumSize = 200 -> 200}, "configurations") at theCenter,
        place(applyButton, "apply") at theSouth
      ) -> "right-panel"
//      (configurations $$ {_.minimumSize = 200 -> 200}) -> "configurations"
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
    self: GenericSimpleAppFrame =>

    class ModifiedBuffer[N] private[FrameExec]( var width: Int,
                                                var height: Int,
                                                var tpe: Int,
                                                var data: Array[N]
                                                 )
    protected case class ImageHolder[T] (width: Int, height: Int, jImageType: Int, data: Array[T])

    object JImageTypeConversions{
      def toCV(jType: Int): Int = jType match {
        case BufferedImage.TYPE_3BYTE_BGR => CvType.CV_8UC3
      }
      def toJ(cvType: Int): Int = cvType match {
        case CvType.CV_8UC3 => BufferedImage.TYPE_3BYTE_BGR
      }
    }

    trait Runner[N]{
      private val modifiedBuffer = new ModifiedBuffer[N]( image.getWidth,
                                                          image.getHeight,
                                                          image.getType,
                                                          getData(image.getData.getDataBuffer))

      protected def getData(buff: DataBuffer): Array[N]

      def exec(): ImageHolder[N]

      def applyConfig() = {
        val res = exec()

        accessBuffer{
          (buf: ModifiedBuffer[N]) =>
            buf.width = res.width
            buf.height = res.height
            buf.tpe = res.jImageType
            buf.data = res.data
        }
        currentModifiedImage = mkImage(snapshot)

        modified.repaint()
      }

      final def accessBuffer[R](f: ModifiedBuffer[N] => R): R = synchronized{ f(modifiedBuffer) }

      def snapshot = accessBuffer((b: ModifiedBuffer[N]) => ImageHolder[N](b.width, b.height, b.tpe, b.data))

      def mkImage(snapshot: ImageHolder[N] = snapshot): BufferedImage
    }

    protected var currentRunner: Runner[_] = getRunner(image)
    private var currentModifiedImage = currentRunner.mkImage()


    def applyAppropriateConfig() = synchronized{ currentRunner.applyConfig() }
//      val (run, img) = synchronized{ currentRunner -> currentModifiedImage }
//      run.exec()


    protected lazy val runners = mutable.Map.empty[ImageType, Runner[_]]

    def getRunner(img: BufferedImage): Runner[_] = imageType(img) |> {
      tpe =>
        runners.getOrElse(tpe, {
          val newRunner = mkRunner(tpe)
          runners += tpe -> newRunner
          newRunner
        })
    }

    protected def mkRunner: ImageType => Runner[_]
//    protected def mkRunner = DoCaseType(caseInt = ???,
//                                        caseByte = ???,
//                                        caseByteArr = ???,
//                                        caseShort = ???)



    def modifiedImage = currentModifiedImage

//    protected def mkImage[N](snapshot: ModifiedBufferSnapshot[N])
//                            (implicit builder: CanMakeBufferedImage[N]) = builder mk snapshot

//      new BufferedImage(snapshot.width, snapshot.height, snapshot.tpe) |>{
//      img =>
//        val from = snapshot.data
//        val to = img.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
//        System.arraycopy(from, 0, to, 0, from.length)
//        img
//    }

    lazy val applyButton = triggerFor(applyAppropriateConfig()).button("Apply")

//    trait CanMakeBufferedImage[From]{
//      def mk(snapshot: ImageHolder[From]): BufferedImage
//    }

  }

  @deprecated
  trait FrameExecMatSupport extends FrameExec {
    self: GenericSimpleAppFrame with MatCreation=>

    @deprecated
    trait MatSupport[T] {
//      implicit def canFillMat: CanFillMat[T]
//
//      implicit def canExtractMat: CanExtractMat[T]

      implicit def matToImageHolder: ToImageHolder[T]
    }

    //    trait RunnerMatSupport[T] extends Runner[T]{
    //
    //    }

    abstract class ToImageHolder[T: ClassTag]
    {
      def convert(mat: Mat): ImageHolder[T]

      protected def convertHelper(mat: Mat, getData: Array[T] => Mat => Unit) = {
        val data = Array.ofDim[T](mat.width * mat.height * mat.channels())
        getData(data)(mat)
        ImageHolder(mat.width, mat.height, mat.`type`(), data)
      }
    }

    trait ToMat[T]{
      def convert(iHolder: ImageHolder[T]): Mat

      protected def convertHelper(iHolder: ImageHolder[T], putData: Array[T] => Mat => Unit) ={
        println("iHolder.tpe = " + iHolder.jImageType)
        new Mat(iHolder.height, iHolder.width, JImageTypeConversions.toCV(iHolder.jImageType)) $$ {
          mat => putData(iHolder.data)(mat)
        }
      }
    }

    def toImageHolder [T](mat: Mat)               (implicit c: ToImageHolder[T]): ImageHolder[T]  = c.convert(mat)
    def toMat         [T](iHolder: ImageHolder[T])(implicit c: ToMat[T])        : Mat             = c.convert(iHolder)
  }

  trait FrameExecRunners extends FrameExec {
    self: GenericSimpleAppFrame with FrameExecMatSupport with MatCreation =>

    def runnerFor(func: Mat => Mat)(tpe: ImageType): Runner[_] = tpe match {
      case ImageType.Int  => new IntRunner(func)
      case ImageType.Byte => new ByteRunner(func)
    }

    protected abstract class RunnerImpl[T: ToMat: ToImageHolder](val func: Mat => Mat) extends Runner[T]
    {
      def fillImg(snapshot: ImageHolder[T], fill: BufferedImage => Unit) =
        new BufferedImage(snapshot.width, snapshot.height, snapshot.jImageType) $$ fill

      def mkImage(snapshot: ImageHolder[T]): BufferedImage =
        new BufferedImage(snapshot.width, snapshot.height, snapshot.jImageType) $$ {
          img =>
            System.arraycopy(snapshot.data, 0, getData(img.getRaster.getDataBuffer), 0, snapshot.data.length)
        }

      def exec() = toImageHolder(func( toMat(snapshot) ))
    }

    class IntRunner(func: Mat => Mat) extends RunnerImpl[Int](func)
    {
      protected def getData(buff: DataBuffer): Array[Int] = buff.asInstanceOf[DataBufferInt].getData
    }

    implicit def matToIntImageHolder: ToImageHolder[Int] = new ToImageHolder[Int] {
      def convert(mat: Mat): ImageHolder[Int] = convertHelper(mat, data => _.get(0, 0, data))
    }
    //      {
    //        val data = Array.ofDim[Int](mat.width * mat.height * mat.depth)
    //        mat.get(0, 0, data)
    //        ImageHolder(mat.width, mat.height, mat.`type`(), data)
    //      }

    implicit def intImageHolderToMat: ToMat[Int] = new ToMat[Int] {
      def convert(iHolder: ImageHolder[Int]): Mat = convertHelper(iHolder, data => _.put(0, 0, data))
    }


    class ByteRunner(func: Mat => Mat) extends RunnerImpl[Byte](func)
    {
      protected def getData(buff: DataBuffer): Array[Byte] = buff.asInstanceOf[DataBufferByte].getData
    }

    implicit def matToByteImageHolder: ToImageHolder[Byte] = new ToImageHolder[Byte] {
      def convert(mat: Mat): ImageHolder[Byte] = convertHelper(mat, data => _.get(0, 0, data))
    }

    implicit def byteImageHolderToMat: ToMat[Byte] = new ToMat[Byte] {
      def convert(iHolder: ImageHolder[Byte]): Mat = convertHelper(iHolder, data => _.put(0, 0, data))
    }

  }


  trait ConfigurationsPanelBuilder{
    frame: GenericSimpleAppFrame =>

    trait SimpleVerticalPanel extends GridBagPanel with GenericConfigurationPanel{
      val elems: Map[String, Seq[Component with UpdateInterface]]
      def updateForms(): Unit = elems.foreach(_._2.foreach(_.updateForm()))

      protected lazy val thePanel = panel.grid(elems.size, 1)(prepareElems: _*)
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
