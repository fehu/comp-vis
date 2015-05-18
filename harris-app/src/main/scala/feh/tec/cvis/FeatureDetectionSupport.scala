package feh.tec.cvis

import java.awt.Color

import breeze.stats.DescriptiveStats
import feh.dsl.swing2.Var
import feh.tec.cvis.common.cv.Helper._
import feh.tec.cvis.common.cv.describe.CallHistory.ArgEntry
import feh.tec.cvis.common.cv.describe.{ArgDescriptor, CallDescriptor, CallHistoryContainer}
import feh.tec.cvis.common.cv.{Drawing, FeatureDetection, FeatureDetectionType, FeatureDetectionTypeModifier, FeatureDetectionTypeRoot}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import feh.util._
import org.opencv.core.{KeyPoint, Mat}
import org.opencv.features2d.FeatureDetector

import scala.collection.mutable

/** ! SIFT and SURF are non-free ! */
trait FeatureDetectionSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper with Drawing with InterestPointSearchSupport =>

  trait FeatureDetectionSupportFrame extends ConfigurationsPanelBuilder with FeatureDetection with InterestPointSearchSupportFrame{
    frame: GenericSimpleAppFrame
      with FrameExec
      with HistorySupport
      with LayoutDSL
      with ConfigBuildHelperGUI =>

    lazy val featureDetectionResult = Var[CallHistoryContainer[List[KeyPoint]]](CallHistoryContainer.empty(Nil))

    trait FeatureDetectionPanel
      extends SimpleVerticalPanel
      with PanelExecHistory[Mat, List[KeyPoint]]
      with ConfigBuildHelperPanel
      with InterestPointSearchPanel
    {
      def steps = 1
      def classTag = scala.reflect.classTag[List[KeyPoint]]

      def setResult: CallHistoryContainer[List[KeyPoint]] => Unit = {
        c =>
          featureDetectionResult set c
          drawFeatureDetectionResult(Some(c.value))
      }

      def callDescriptor: CallDescriptor[List[KeyPoint]] = CallDescriptor("SIFT")

      def params: Set[ArgEntry[_]] = Set()


      protected def detectorsUnsupported = Set(feature.Star, feature.SURF, feature.SIFT, feature.Dense)
      protected def detectorsSupported = feature.list.filterNot(detectorsUnsupported.contains)

      val detectorType      = Var[FeatureDetectionType]        (detectorsSupported.head)
      val detectorModifier  = Var[FeatureDetectionTypeModifier](feature.modifier.None)

      object DetectorType     extends ArgDescriptor[FeatureDetectionType]        ("Feature detector", null)
      object DetectorModifier extends ArgDescriptor[FeatureDetectionTypeModifier]("Feature detector modifier", null)

      lazy val detectorTypeChooser      = mkListControl(DetectorType,     detectorsSupported)   (detectorType.set,     _.toString)
      lazy val detectorModifierChooser  = mkListControl(DetectorModifier, feature.modifier.list)(detectorModifier.set, _.toString)


      def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "detectorType"      -> detectorTypeChooser
      , "detectorModifier"  -> detectorModifierChooser
      )


      private val detectors = mutable.HashMap.empty[FeatureDetectionTypeRoot, FeatureDetector]

      def getDetector(tpe: FeatureDetectionTypeRoot): FeatureDetector = detectors.getOrElseUpdate(tpe, create(tpe))

      def runner: Runner[Params, Mat, List[KeyPoint]] = Runner{
        _ => _ => src =>
          withGrayImg(src){
            grayImg =>
              val dtpe = detectorModifier.get :: detectorType.get
              println("detector type " + dtpe + " -- " + dtpe.value)
              val detector = getDetector(dtpe)
              detector.detect(grayImg).toBuffer.toList
          }
      }

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted


      def drawFeatureDetectionResult(resOpt: Option[List[KeyPoint]]): Unit = {
        if(repaint_?.get) Option(originalInGrayScale) map (_.clone()) foreach setImageMat
        affectImageMat { img =>
          def draw(color: Color)(kp: KeyPoint) = img.draw.circle(kp.pt, kp.size.toInt + 1, color)

          val res = resOpt getOrElse featureDetectionResult.get.value //.sortBy(_.response)
          val resp = res.map(_.response.toDouble)

          val ordered = Y[(List[KeyPoint], List[Double]), List[List[KeyPoint]]](
            rec => {
              case (last, Nil) => last :: Nil
              case (Nil, _) => Nil
              case (list, p :: ps) =>
                val prc =  DescriptiveStats.percentile(resp, p)
                val (hs, ts) = list.partition(_.response < prc)
                hs :: rec(ts, ps)
            }
          )(res -> List(0.25, 0.5, 0.75))

          println("ordered = " + ordered)

          ordered zip Seq(Color.green, Color.cyan, Color.red, Color.orange) foreach {
            case (l, c) => l foreach draw(c)
          }
        }
        repaintImage()
      }
    }
  }
}
