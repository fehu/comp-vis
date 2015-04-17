package feh.tec.cvis.gui

import java.awt.{Color, Dimension}
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
          .affect(_.border = Swing.LineBorder(Color.red))

    }

  }
}
