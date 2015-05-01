package feh.tec.cvis.testapp

import java.awt.{Color, Dimension}
import java.awt.image._
import javax.swing.SpinnerNumberModel
import feh.tec.cvis.common.Helper._
import feh.tec.cvis.common._
import feh.tec.cvis.common.describe.{Harris, ConvertColor, CallDescriptor}
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.Harris
import feh.tec.cvis.gui.configurations.{GuiArgModifier, Harris}
import feh.util._
import org.opencv.core.{CvType, TermCriteria, Core, Mat}
import scala.swing.Component
import scala.swing.Swing._
import Drawing._

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

      protected def applyMat: Mat = originalMat

      type Config = SimpleVerticalPanel with HarrisConfigurationPanelExec

      lazy val configurations: Config = new SimpleVerticalPanel with HarrisConfigurationPanelExec{
        var threshold: Double = 0.1

        lazy val thresholdControl = controlForOrdered(threshold)(threshold = _)
                                    .spinner(new SpinnerNumberModel(threshold, 0, Double.PositiveInfinity, 0.001))

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
                                                   .filter(_._2 > threshold)
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
                .filter(_._2 > threshold)
        }

        override def formBuilders: Seq[(String, (TestHarris.DSLFormBuilder[_], TestHarris.DSLLabelBuilder[_]))] =
          super.formBuilders/*.filterNot(_._1 == "k")*/ ++ Seq(
            "responseFunc" -> (responseFuncControl, label("Response Function")),
            "threshold" -> (thresholdControl -> label("Threshold"))
          )

        lazy val elems: Seq[(String, Seq[Component with UpdateInterface])] =
          formBuilders.mapVals{
            case (form, label) => label.formMeta.form :: form.formMeta.form :: Nil
          }

        def kStep = Some(GuiArgModifier.Step(0.001))

//        override lazy val runner: Runner[Params, Mat, Mat] = Runner(
//          params =>
//            CallDescriptor.WithScopeAndParams(ConvertColor.Descriptor, frame,
//                                              (ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None)) chain
//            CallDescriptor.WithScopeAndParams(Harris.Descriptor, frame,
//                                              (blockSize, kSize, k.toDouble, Option(borderType))) chain
//            CallDescriptor.Callable()
//        )

//        var initialNClusters = 250
        var nClustersStep = 10
        var nClustersMaxTries = 100

        var criteria = TerminationCriteria(_.Count, 1000, 1e-5)
        var attempts = 100
        var centersInitialPolicy: CentersPolicy = CentersPolicy.Random

        var maxCompactness = 1e4

        override lazy val runner: Runner[Params, Mat, Mat] = Runner {
          params =>
            src =>
              val cvt = ColorConversion(imageColorType, ColorMode.Gray)
              cvtColor(src, cvt) |> {
                grayImg =>

                  val filtered = responseFunc.fromGray(grayImg)
                  println("filtered.length = " + filtered.length)

                  val res = grayImg.convert(cvt.inverse)

                  filtered.foreach{ case ((i, j), r) => res.draw.circle(j -> i, 1, Color.red) }

                  lazy val initialNClusters = filtered.length / 20

                  if(filtered.length >= initialNClusters){
                    val cData = filtered.toMatOfPoint.convert(CvType.CV_32F).t()

                    val best = new Mat()
                    def centersPolicy = if(best.empty()) centersInitialPolicy
                                        else CentersPolicy.InitialLabels
                    def kMeans(nClusters: Int) = {
                      println("kMeans")
                      kmeans(cData, nClusters, criteria, attempts, centersPolicy, best)
                    }

                    val KMeansResult(ccenters, bestLabels, compactness) = {
                      println("search KMeansResult")
                      doUntil(initialNClusters, nClustersMaxTries)(
                        nClust =>
                          kMeans(nClust) |> {
                            res =>
                              println(s"kmeans with $nClust clusters: compactness = ${res.compactness}")

                              if(res.compactness <= maxCompactness) scala.Right(res)
                              else                                  scala.Left(nClust+nClustersStep)
                          }).right
                      .getOrElse(sys.error(s"coulndn't find clusters with max compactness $maxCompactness in $nClustersMaxTries tries"))
                    }

                    println("bestLabels = " + bestLabels)
                    println("compactness = " + compactness)

                    ccenters.map(_.swap).foreach(res.draw.circle(_, 5, Color.blue))
                  }

                  res
              }



        }

      }

      frame.updateForms()
    }

  def doUntil[T, R](initial: T, maxTry: Int = 100)(f: T => Either[T, R]): Either[T, R] =
    Y[(T, Int), Either[T, R]](
      rec => {
        case (prev, c) =>
          f(prev) match {
            case Left(t) => if (c == maxTry)  Left(t)
                            else              rec(t -> (c+1))
            case right   => right
          }
      }
    )(initial -> 0)
}
