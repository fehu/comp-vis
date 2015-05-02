package feh.tec.cvis.testapp

import java.awt.{Color, Dimension}
import java.awt.image._
import javax.swing.SpinnerNumberModel
import feh.tec.cvis.common.Helper._
import feh.tec.cvis.common._
import feh.tec.cvis.common.describe.ArgModifier.MinCap
import feh.tec.cvis.common.describe._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.{GuiArgModifier, Harris}
import feh.util._
import org.opencv.core._
import scala.collection.mutable
import scala.swing.{Dialog, Component}
import scala.swing.Swing._
import Drawing._
import PointNumericImplicits._

object TestHarris extends DefaultApp("harris-test", 300 -> 300, 600 -> 800) with Harris{

  CV.loadNative()

  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit ) =
    new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
      with ConfigurationsPanelBuilder
      with HarrisGUI
      with FrameExec
      with CornerDetection
      with MatSupport
      with ColorConverting
      with Clustering
    {
      frame =>

//      LayoutDebug = true

      protected def applyMat: Mat = originalMat

      type Config = SimpleVerticalPanel with PanelExec[_, _]

      protected var originalGray: Mat = null

      protected var harrisResult  : List[((Int, Int), Double)] = Nil
      protected var harrisFiltered: List[((Int, Int), Double)] = Nil

      protected var filteredInterestPointsCount: Int = 0
      protected var initialNClusters: Int = 1

      protected var clusterCentersAndWeight = List.empty[(Point, Double)]

      lazy val configurations: Seq[(String, Config)] = Seq(
        "harris"      -> HarrisPanel
//        "clustering"  -> ClusteringPanel
      )

      def calcInitialNClusters(c: Int) =  if      (c > 1000)  c / 100
                                          else if (c > 10)    c / 10
                                          else                1

      object HarrisPanel extends SimpleVerticalPanel with HarrisConfigurationPanelExec{
        panel =>

        def steps = 2

        def kStep = Some(GuiArgModifier.Step(0.001))

        def getSrc = originalMat
        def setResult: Mat => Unit = _ => drawHarris()

        var threshold: Double = 0.1
        lazy val thresholdControl = controlForOrdered(threshold)(threshold = _)
                                    .spinner(new SpinnerNumberModel(threshold, 0, Double.PositiveInfinity, 0.001))

        lazy val applyThresholdButton = triggerFor{
          setHarrisFiltered( filterHarris(harrisResult) )
          drawHarris()
          tabs.tryUpdate()
        }.button("Apply Threshold")

        var responseFunc: ResponseFunc = ResponseFunc.Original
        lazy val responseFuncControl = controlForSeq(ResponseFunc.all,  static = true)
                                       .dropDownList{
                                                      rf =>
                                                        responseFunc = rf
                                                        componentAccess.get("k").get.visible = rf == ResponseFunc.Original
                                                    }

        case class ResponseFunc(name: String, fromGray: Mat => Stream[((Int, Int), Double)]){
          override def toString = name
        }

        object ResponseFunc{
          def all = Original :: DetTrace :: DetTraceSq :: Nil

          object Original extends ResponseFunc("det - k*trace^2",
                                               grayImg =>
                                                 cornerHarris(grayImg, blockSize, kSize, k.toDouble, Option(borderType))
                                                 .mapV { case Array(d) => d }
                                                 .lazyPairs
          )
          object DetTrace   extends ResponseFunc("det / trace",   withEigenValues(cornerResponseDetTrace))
          object DetTraceSq extends ResponseFunc("det / trace^2", withEigenValues(cornerResponseDetTraceSq))

          def cornerResponseDetTrace:   EigenValsAndVecs => Double    =   eign => eign.det / eign.trace
          def cornerResponseDetTraceSq: EigenValsAndVecs => Double    =   eign => eign.det / math.pow(eign.trace, 2)

          def withEigenValues(response: EigenValsAndVecs => Double) =
            (grayImg: Mat) =>
              cornerEigenValsAndVecs(grayImg, blockSize, kSize, Option(borderType))
              .lazyPairs
              .mapVals(response)
        }

        def setHarrisFiltered(seq: Seq[((Int, Int), Double)]) = {
          harrisFiltered = seq.toList
          filteredInterestPointsCount = harrisFiltered.length
          initialNClusters = calcInitialNClusters(filteredInterestPointsCount)
        }

        override lazy val runner: Runner[Params, Mat, Mat] = Runner {
          nextStep =>
            params =>
              src =>
                val cvt = ColorConversion(imageColorType, ColorMode.Gray)
                cvtColor(src, cvt) |> {
                  grayImg =>
                    originalGray = grayImg.convert(cvt.inverse)
                    nextStep()
                    val responses = responseFunc.fromGray(grayImg)
                    harrisResult = responses.toList            // TODO !!! no side effects should be present here
                    nextStep()
                    setHarrisFiltered( filterHarris(harrisResult) )
                  grayImg
                }
        }

//        override lazy val runner: Runner[Params, Mat, Mat] = Runner(
//          params =>
//            CallDescriptor.WithScopeAndParams(ConvertColor.Descriptor, frame,
//                                              (ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None)) chain
//            CallDescriptor.WithScopeAndParams(Harris.Descriptor, frame,
//                                              (blockSize, kSize, k.toDouble, Option(borderType))) chain
//            CallDescriptor.Callable()
//        )

        protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

        def filterHarris: Seq[((Int, Int), Double)] => Seq[((Int, Int), Double)] = _.filter(_._2 >= threshold)

        lazy val showFilteredInterestPointsCount  = monitorFor(s"Filtered interest points: $filteredInterestPointsCount").text

        override def formBuilders: Seq[(String, (TestHarris.DSLFormBuilder[_], TestHarris.DSLLabelBuilder[_]))] =
          super.formBuilders ++ Seq(
            "responseFunc"                  -> (responseFuncControl -> label("Response Function")),
            "threshold"                     -> (thresholdControl    -> label("Threshold")),
            "applyThreshold"                -> (applyThresholdButton            -> label("")),
            "filteredInterestPointsCount"   -> (showFilteredInterestPointsCount -> label(""))
          )

        def drawHarris() = {
          Option(originalGray) map (_.clone()) foreach setImageMat
          affectImageMat(img => harrisFiltered.foreach{ case ((i, j), r) => img.draw.circle(j -> i, 1, Color.red) })
          repaintImage()
        }
      }

//      List[((Int, Int), Double)]


      frame.updateForms()
    }

  /*
          def drawClusterCenters()  = {
          val centers = ???

          affectImageMat(img => centers.foreach(p => img.draw.circle(p.swap, 5, Color.blue)))

          val neighbouringMap =(
            for{
              point   <- centers
              another <- centers
              if another != point
              dist = point.distance[EuclideanDistance](another)
              if dist < 10
            } yield point -> another
          ).groupBy(_._1)
           .mapValues(_.map(_._2))

//          val (single, multiple) = neighbouringMap.partition(_._2.length < 2)

          val groups = {
            val g = mutable.Buffer.empty[mutable.HashSet[Point]] //++= neighbouringMap.keys

            for( (point, neighbours) <- neighbouringMap )
              g.find(_ contains point)
               .map(_ ++= neighbours)
               .getOrElse( g += (new mutable.HashSet += point ++= neighbours) )

            val cCenters = for (grouped <- g) yield grouped.sum / (grouped.size -> grouped.size)

            affectImageMat(img => cCenters.foreach(p => img.draw.circle(p.swap, 5, Color.green)))
          }

          repaintImage()
        }
   */
}
