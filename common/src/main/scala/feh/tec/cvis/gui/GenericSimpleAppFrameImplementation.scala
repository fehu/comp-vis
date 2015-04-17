package feh.tec.cvis.gui

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.swing._
import feh.util._
import feh.util.file._
import feh.tec.cvis.gui.FileDropped._

trait GenericSimpleAppFrameImplementation extends GenericSimpleApp{

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
                   onEmptyClose: => Unit
                    )
    extends Frame with NewFrameOnFileDrop
  {
    title = frameTitle
    contents = new Label(emptyText)

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
  }

  abstract class SimpleFrame(val image: BufferedImage,
                    frameTitle: String,
                    protected val defaultSize: Dimension,
                    protected val regNewFrame: SimpleFrame => Unit,
                    protected val unregAFrame: SimpleFrame => Unit
//                    val configurationsElem: LayoutElem
                     )
    extends GenericSimpleAppFrame
    with NewFrameOnFileDrop
    with Frame9PositionsLayoutBuilder
  {
    type Preview = SimplePreview

//    def mkConfig: Config

    val original: Preview = new SimplePreview{ def img = image }
    val modified: Preview = new SimplePreview{ def img = image }  // todo
//    val configurations: Config = componentAccess.get(configurationsElem.id).asInstanceOf[Config]
    //    val configurations: Config = new Panel{ _contents += new Label("config") }

    this.title = frameTitle

//    val configurations: Config = mkConfig

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
      panel.grid(2, 1)(original -> "image-original", modified -> "image-modified") -> "image-previews",
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

  def mkSimpleFrame(params: MkSimpleFrameParams): SimpleFrame = params.image match {
    case Left(img)   => mkSimpleFrame(img,  params.frameTitle, params.defaultSize, params.regNewFrame, params.unregAFrame)
    case Right(file) => mkSimpleFrame(file, params.frameTitle, params.defaultSize, params.regNewFrame, params.unregAFrame)
  }


  case class MkSimpleFrameParams(image: Either[BufferedImage, File],
                                 frameTitle: String,
                                 defaultSize: Dimension,
                                 regNewFrame: SimpleFrame => Unit,
                                 unregAFrame: SimpleFrame => Unit)


  //  object SimpleFrame{
//    def apply[C <: GenericConfigurationPanel](
//              image: BufferedImage,
//              frameTitle: String,
//              defaultSize: Dimension,
//              regNewFrame: SimpleFrame => Unit,
//              unregAFrame: SimpleFrame => Unit,
//              mkConf: => C): SimpleFrame{ type Config = C } =
//      new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame){
//        type Config = C
//        def mkConfig = mkConf
//      }
//
//    def apply[C <: SimpleFrame#Config](
//              file: File,
//              frameTitle: String,
//              defaultSize: Dimension,
//              regNewFrame: SimpleFrame => Unit,
//              unregAFrame: SimpleFrame => Unit,
//              mkConf: => C): SimpleFrame =
//    {
//      val img = file.withInputStream(ImageIO.read).get
//      new SimpleFrame(img, frameTitle, defaultSize, regNewFrame, unregAFrame){
//        type Config = C
//        def mkConfig = mkConf
//      }
//    }
//  }

  trait ConfigurationsPanelBuilder{
    frame: GenericSimpleAppFrame =>

    trait SimpleVerticalPanel extends GenericConfigurationPanel{
      val elems: Map[String, DSLFormBuilder[_]#FormBuildMeta]
      def updateForms(): Unit = elems.foreach(_._2.form.updateForm())

      _contents += panel.box(_.Vertical)(elems.mapValues(_.component).toSeq.map(_.swap): _*)
    }

//    def simpleVertical(elems: Map[String, DSLFormBuilder[_]]): GenericConfigurationPanel = new GenericConfigurationPanel{
//      def updateForms(): Unit = ???
//
//      def updateImage: (Array[Array[Byte]]) => Unit = ???
//
//      _contents += panel.box(_.Vertical)(elems.mapValues(_.formMeta.form).toSeq.map(_.swap): _*)
//    }
  }
}
