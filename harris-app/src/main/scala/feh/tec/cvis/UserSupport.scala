package feh.tec.cvis

import java.awt.Color
import java.awt.image.BufferedImage
import java.util.UUID
import javax.swing.JSpinner

import feh.tec.cvis.common.cv._
import feh.tec.cvis.common.cv.describe.{CallHistoryContainer, ArgModifier, ArgDescriptor}
import feh.tec.cvis.gui.configurations.GuiArgModifier.Step
import feh.util._
import feh.dsl.swing2.{Monitor, Var, Control}
import feh.tec.cvis.DescriptorsSupport.{IDescriptor, ADescriptor}
import feh.tec.cvis.gui.{PreviewMouseReaction, PreviewHighlights, SimplePreview, GenericSimpleAppFrameImplementation}
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import org.opencv.core.{Mat, Point}
import feh.tec.cvis.common.cv.Helper._

import scala.collection.mutable
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
      with PanelExecSimple[CallHistoryContainer[Map[Point, ADescriptor]], Matches]
      with ConfigBuildHelperPanel
      with ColorConverting
    {

      def steps = getSrc.value.size

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

      lazy val showCurrentMatchTrigger       = triggerFor(showCurrentMatch())               .button("Current match")
      lazy val showAllMatchesForImageTrigger = triggerFor(showAllMatchesForImage())         .button("All matches for image")
      lazy val showCrossCorrelationTrigger   = triggerFor(showCrossCorrelationsForImage())  .button("Cross-correlation matches")

      lazy val showPanel = panel.box(_.Horizontal)( showCurrentMatchTrigger       -> noId
                                                  , showAllMatchesForImageTrigger -> noId
                                                  , showCrossCorrelationTrigger   -> noId
                                                  )

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
      
      case class ImageFrame(img: BufferedImage, pointsOfInterest: Set[(Int, Int)], hRadius: Int) extends Frame{

        protected def highlighting(points: Set[scala.swing.Point]) = {}
        protected def stoppingHighlighting() = {}

        contents = new SimplePreview with PreviewHighlights with PreviewMouseReaction{
          def img = ImageFrame.this.img

          def highlightColor = Color.yellow

          protected lazy val highlightCache = pointsOfInterest.flatMap{
            case p@(pX, pY) => for{
                x <- -hRadius to hRadius
                y <- -hRadius to hRadius
                if x*x + y*y < hRadius*hRadius
              } yield p -> (pX + x, pY + y)
          }.groupBy(_._1)
           .mapValues(_.map(_._2))

          def onMouseMovement = {
            case (p, _) =>
              highlightCache.find(_._2 contains (p.y, p.x)).map{
                case (_, points) =>
                  val pts = points.map(x => x: scala.swing.Point)
                  highlights set pts
                  highlighting(pts)
                  this.repaint()
              }.getOrElse{
                if (highlights.get.nonEmpty) {
                  highlights set Set()
                  stoppingHighlighting()
                  this.repaint()
                }
              }
          }
        }
      }

      protected def currentSelection = foundInfo.component |> {
        c => c.selection.rows.map(
          row => c.peer.getModel.getValueAt(row, 0).asInstanceOf[UUID] -> c.peer.getModel.getValueAt(row, 1).asInstanceOf[(Int, Int)]
        )
      }
      
      def imageFrameHighlightActivationRadius = 3

      def showCurrentMatch() =
        currentSelection.par.foreach{
          case (id, point) =>
            val descr = fetchDescriptor(id)
            val ((_, name), ms) = matches.get(point).find(_._1._1 == id).get

            val mat = imageToMat(descr)
            val gray  = toGray(mat, BufferedImageColor.mode(descr.javaType))
            ms.foreach(p => gray.draw.circle(p.swap, 2, Color.red, thickness = 2))

            val img = toBufferImage(gray)

            ImageFrame(img, ms.toSet, imageFrameHighlightActivationRadius).open()
        }

      def showAllMatchesForImage() = currentSelection.groupBy(_._1).par.foreach{
        case (id, _) =>
          val descr = fetchDescriptor(id)
          val gray  = toGray(imageToMat(descr), BufferedImageColor.mode(descr.javaType))
          val ms    = matches.get.values.flatMap(_.filter(_._1._1 == id).values).flatten.toList.distinct

          ms.foreach{
            p => gray.draw.circle(p.swap, 2, Color.red, thickness = 2)
          }
          ImageFrame(toBufferImage(gray), ms.toSet, imageFrameHighlightActivationRadius).open()
      }


      def showCrossCorrelationsForImage() = currentSelection.groupBy(_._1).par.foreach{
        case (id, _) =>
          val descrCurr  = getSrc.value
          val descrOther = fetchDescriptor(id)

          assert(descrCurr.head._2.channelsCount == descrOther.descriptorChannels, "cannot compare")
          assert(descrCurr.head._2.sideLength    == descrOther.sideLength,         "cannot compare")

          val nccC = mutable.HashMap.empty[Point, mutable.HashMap[Point, Double]]
          val nccO = mutable.HashMap.empty[Point, mutable.HashMap[Point, Double]]

          val n = descrOther.sideLength * descrOther.sideLength

          for{
            (ipC, descrC) <- descrCurr
            (ipO, descrO) <- descrOther.interestPoints
          } {
            val sumElems = for {
              pC <- descrC.data
              pO <- descrO.data
            } yield (pC - descrC.mean) / descrC.std * (pO - descrO.mean) / descrO.std

            val crossCorrelation = 1.0 / (n-1) * sumElems.sum
            nccC.getOrElseUpdate(ipC, new mutable.HashMap()) += ipO -> crossCorrelation
            nccO.getOrElseUpdate(ipO, new mutable.HashMap()) += ipC -> crossCorrelation
          }

          val nccBestC = nccC.mapValues(_.maxBy(_._2))
          val nccBestO = nccO.mapValues(_.maxBy(_._2))

          val nccBestMatches = mutable.HashMap.empty[Point, Point]

          for{
            (curr, (other, _)) <- nccBestC
            if nccBestO(other)._1 == curr
          } nccBestMatches += curr -> other

          println("nccBestMatches = " + nccBestMatches)

          val nccBestMatchesInv = nccBestMatches.map(_.swap).toMap

          val gray  = toGray(imageToMat(descrOther), BufferedImageColor.mode(descrOther.javaType))

          nccBestMatchesInv.keySet.foreach{ p => gray.draw.circle(p.swap, 2, Color.blue, thickness = 2) }

          val pts = nccBestMatchesInv.keySet.map(_.pairInt)

          new ImageFrame(toBufferImage(gray), pts, imageFrameHighlightActivationRadius){
            override protected def highlighting(points: Set[scala.swing.Point]) = {
              modified.asInstanceOf[SimplePreview with PreviewHighlights].highlights set points
              modified.repaint()
            }
            override protected def stoppingHighlighting() = {
              modified.asInstanceOf[SimplePreview with PreviewHighlights].highlights set Set()
              modified.repaint()
            }

          }.open()
      }

      
      protected def imageToMat(d: IDescriptor) = new Mat(d.originalSize, d.matType) $${ _.put(0, 0, d.originalImage) }

      protected def toGray(mat: Mat, from: ColorMode) = ColorConversion(from, ColorMode.Gray) |>{
        cv => mat.convert(cv).convert(cv.inverse)
      }
      
      
      

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      def runner: Runner[Params, CallHistoryContainer[Map[Point, ADescriptor]], Matches] = Runner(
        nextStep => {
          case (sOpts, sPrec) =>
            _.value.map{
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
