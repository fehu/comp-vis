package feh.tec.cvis

import java.awt.Color
import java.awt.image.BufferedImage
import java.util.UUID
import javax.swing.JSpinner

import feh.tec.cvis.common.cv._
import feh.tec.cvis.common.cv.describe.{ArgModifier, ArgDescriptor}
import feh.tec.cvis.gui.configurations.GuiArgModifier.Step
import feh.util._
import feh.dsl.swing2.{Monitor, Var, Control}
import feh.tec.cvis.DescriptorsSupport.{IDescriptor, ADescriptor}
import feh.tec.cvis.gui.{SimplePreview, GenericSimpleAppFrameImplementation}
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import org.opencv.core.{Mat, Point}
import feh.tec.cvis.common.cv.Helper._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}
import scala.swing._

trait UserSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper with Drawing =>

  trait UserSupportFrame extends ConfigurationsPanelBuilder {
    frame: GenericSimpleAppFrame with FrameExec with LayoutDSL with ConfigBuildHelperGUI =>

    type Matches = Map[(Int, Int), Map[(UUID, String), Seq[(Int, Int)]]]

    lazy val matches: Var[Matches] = Var(Map())

    trait UserPanel
      extends SimpleVerticalPanel
      with PanelExec[Map[Point, ADescriptor], Matches]
      with ConfigBuildHelperPanel
      with ColorConverting
    {

      def steps = getSrc.size

      def setResult: Matches => Unit = matches.set
      def classTag = scala.reflect.classTag[Matches]


      def searchDb( mean      : Option[Double]
                  , std       : Option[Double]
                  , range     : Option[Double]
                  , iqr       : Option[Double]
                  , precision : Double
                  ) : Future[ Map[(UUID, String), Seq[(Int, Int)]] ]

      def searchDbTimeout: FiniteDuration


      type Params = (SearchOptions, SearchPrecision)
      def getParams(): Params = ( SearchOptions(searchByMean.get, searchByStd.get, searchByRange.get, searchByIqr.get)
                                , SearchPrecision(searchPrecision)
                                )

      case class SearchOptions(byMean: Boolean, byStd: Boolean, byRange: Boolean, byIqr: Boolean)
      case class SearchPrecision(precision: Double)


      protected lazy val searchByMean  = Var(true)
      protected lazy val searchByStd   = Var(false)
      protected lazy val searchByRange = Var(false)
      protected lazy val searchByIqr   = Var(true)

      protected var searchPrecision = 1e-7


      object SearchPrecision extends ArgDescriptor[Double]("search precision", null, ArgModifier.Positive, Step(1e-7))


      lazy val searchByMeanControl  = Control(searchByMean,   new CheckBox("mean"))
      lazy val searchByStdControl   = Control(searchByStd,    new CheckBox("stdev"))
      lazy val searchByRangeControl = Control(searchByRange,  new CheckBox("range"))
      lazy val searchByIqrControl   = Control(searchByIqr,    new CheckBox("iqr"))

      lazy val searchByPanel = panel.grid(4, 1)( searchByMeanControl.component  -> "searchByMean"
                                               , searchByStdControl.component   -> "searchByStd"
                                               , searchByRangeControl.component -> "searchByRange"
                                               , searchByIqrControl.component   -> "searchByIqr"
                                               )

      lazy val searchPrecisionControl = mkNumericControl(SearchPrecision)(searchPrecision, searchPrecision = _) |> fixPreferredSize

      lazy val showCurrentMatchTrigger       = triggerFor(showCurrentMatch)         .button("Current match")
      lazy val showAllMatchesForImageTrigger = triggerFor(showAllMatchesForImage()) .button("All matches for image")

      lazy val showPanel = panel.box(_.Horizontal)(showCurrentMatchTrigger -> noId, showAllMatchesForImageTrigger -> noId)

      lazy val foundInfo = Monitor.forTable(matches, new Table())(
        columns = "UUID" :: "Point" :: "Matched Image" :: "Matched Points" :: Nil
      , initial = Map()
      , prepareData = _.toArray.map {
                                      case (p, m) => m.flatMap{
                                        case ((id, name), pms) => Seq[AnyRef](id, p, name, pms.mkString(", "))
                                      }.toArray
                                    }
      )(c => c.peer.removeColumn(c.peer.getColumnModel.getColumn(0)))


      lazy val formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "searchByPanel"   -> (searchByPanel -> label("Search by"))
      , "searchPrecision" -> {
          val spinner = searchPrecisionControl._1.peer.asInstanceOf[JSpinner]
          spinner.setEditor{ new JSpinner.NumberEditor(spinner, "0.################") }
          searchPrecisionControl
        }
      )

      override lazy val elems: Seq[(String, Seq[Component])] = mkElems ++ Seq(
        "showPanel" -> Seq(label("<html><h3>Show</h3></html>").component, showPanel.component)
      , "foundInfo" -> Seq(label("Preliminary matches").component, new ScrollPane(foundInfo.component))
      )

      
      
      
      def fetchDescriptor(id: UUID): IDescriptor
      
      case class ImageFrame(img: BufferedImage) extends Frame{
        contents = new SimplePreview {
          def img: BufferedImage = ImageFrame.this.img
        }
      }

      protected def currentSelection = foundInfo.component |> {
        c => c.selection.rows.map(
          row => c.peer.getModel.getValueAt(row, 0).asInstanceOf[UUID] -> c.peer.getModel.getValueAt(row, 1).asInstanceOf[(Int, Int)]
        )
      }

      def showCurrentMatch() = {
        val sel = currentSelection

        sel.par.foreach{
          case (id, point) =>
            val descr = fetchDescriptor(id)
            val ((_, name), ms) = matches.get(point).find(_._1._1 == id).get

            val mat = new Mat(descr.originalSize.height.toInt, descr.originalSize.width.toInt, descr.matType)
            mat.put(0, 0, descr.originalImage)
            val cv = ColorConversion(BufferedImageColor.mode(descr.javaType), ColorMode.Gray)
            val mc = mat.convert(cv).convert(cv.inverse)
            ms.foreach(p => mc.draw.circle(p, 2, Color.red, thickness = 2))

            val img = toBufferImage(mc)

            ImageFrame(img).open()
        }
      }
      
      def showAllMatchesForImage() = ???
      
      
      
      
      
      

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      def runner: Runner[Params, Map[Point, ADescriptor], Matches] = Runner(
        nextStep => {
          case (sOpts, sPrec) =>
            _.map{
                case (p, ad) =>
                  val mean  = if(sOpts.byMean)  Some(ad.mean)   else None
                  val std   = if(sOpts.byStd)   Some(ad.std)    else None
                  val range = if(sOpts.byRange) Some(ad.range)  else None
                  val iqr   = if(sOpts.byIqr)   Some(ad.iqr)    else None
                  val res   = Await.result(searchDb(mean, std, range, iqr, sPrec.precision), searchDbTimeout)
                  nextStep()
                  p.pairInt -> res
            }.filter(_._2.nonEmpty)



        }
      )

    }

  }
}
