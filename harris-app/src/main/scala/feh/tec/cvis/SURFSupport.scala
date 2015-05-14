package feh.tec.cvis

import java.awt.Color

import breeze.stats.DescriptiveStats
import feh.dsl.swing2.Var
import feh.tec.cvis.common.cv.{Drawing, FeatureDetection}
import feh.tec.cvis.common.cv.describe.CallHistory.ArgEntry
import feh.tec.cvis.common.cv.describe.{CallDescriptor, CallHistoryContainer}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import org.opencv.core.{KeyPoint, Mat, MatOfKeyPoint}
import feh.tec.cvis.common.cv.Helper._
import feh.util._

/** ! SURF is non-free ! */
trait SURFSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper with Drawing with InterestPointSearchSupport =>

  trait SURFSupportFrame extends ConfigurationsPanelBuilder with FeatureDetection with InterestPointSearchSupportFrame{
    frame: GenericSimpleAppFrame
      with FrameExec
      with HistorySupport
      with LayoutDSL
      with ConfigBuildHelperGUI =>

    lazy val surfResult = Var[CallHistoryContainer[List[KeyPoint]]](CallHistoryContainer.empty(Nil))

    trait SURFPanel
      extends SimpleVerticalPanel
      with PanelExecHistory[Mat, List[KeyPoint]]
      with ConfigBuildHelperPanel
      with InterestPointSearchPanel
    {
      def steps = 1
      def classTag = scala.reflect.classTag[List[KeyPoint]]

      def setResult: CallHistoryContainer[List[KeyPoint]] => Unit = {
        c =>
          surfResult set c
          drawSURFResult()
      }

      def callDescriptor: CallDescriptor[List[KeyPoint]] = CallDescriptor("SIFT")

      def params: Set[ArgEntry[_]] = Set()



      def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Nil


      lazy val detector = create(feature.Harris)

      def runner: Runner[Params, Mat, List[KeyPoint]] = Runner{
        _ => _ => src =>
          withGrayImg(src){
            grayImg => detector.detect(grayImg).toBuffer.toList
          }
      }

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted


      def drawSURFResult(): Unit ={
        affectImageMat { img =>
          def draw(color: Color)(kp: KeyPoint) = img.draw.circle(kp.pt, kp.size.toInt + 1, color)

          val surf = surfResult.get.value //.sortBy(_.response)
          val surfResp = surf.map(_.response.toDouble)

          val surfOrdered = Y[(List[KeyPoint], List[Double]), List[List[KeyPoint]]](
            rec => {
              case (last, Nil) => last :: Nil
              case (Nil, _) => Nil
              case (list, p :: ps) =>
                val prc =  DescriptiveStats.percentile(surfResp, p)
                val (hs, ts) = list.partition(_.response < prc)
                hs :: rec(ts, ps)
            }
          )(surf -> List(0.25, 0.5, 0.75))

          println("surfOrdered = " + surfOrdered)

          surfOrdered zip Seq(Color.green, Color.cyan, Color.red, Color.orange) foreach {
            case (l, c) => l foreach draw(c)
          }
        }

      }
    }
  }
}
