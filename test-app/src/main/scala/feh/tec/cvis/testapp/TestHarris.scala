package feh.tec.cvis.testapp

import java.awt.Dimension
import java.awt.image._

import feh.dsl.swing2.Var
import feh.tec.cvis.common.Helper._
import feh.tec.cvis.common._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.{GrouppingSupport, HarrisSupport, KMeansSupport}
import org.opencv.core._

import scala.swing.Swing._

object TestHarris extends DefaultApp("harris-test", 300 -> 300, 600 -> 800) 
  with HarrisSupport 
  with KMeansSupport
  with GrouppingSupport
{

  CV.loadNative()

  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit ) =
    new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
      with ConfigurationsPanelBuilder
      with FrameExec
      with HarrisSupportFrame
      with KMeansSupportFrame
      with GrouppingSupportFrame
      with CornerDetection
      with MatSupport
    {
      frame =>

//      LayoutDebug = true

      protected def applyMat: Mat = originalMat

      type Config = SimpleVerticalPanel with PanelExec[_, _]


      lazy val configurations: Seq[(String, Config)] = Seq(
          "harris"      -> HarrisPanel
        , "grouping"    -> GroupingPanel
        , "k-means"     -> KMeansPanel
      )

      object GroupingPanel extends GroupingPanel{

        def getSrc = harrisFiltered

        override def drawGroupsCenters(): Unit = {
          KMeansPanel.getInitialLabels set {              // todo: should be in another place, not in draw
            val inGroups = groupsCentersWithPoints.map(_._2)
            val outOfGroups = harrisFiltered
                                .withFilter(x => !inGroups.exists(_.contains(x._1)))
                                .map{
                                  case (p, _) => Set(p: Point)
                                }
            inGroups ++ outOfGroups
          }

          if(repaint_?.get) HarrisPanel.drawHarris()
          super.drawGroupsCenters()
        }
      }
      
      object KMeansPanel extends KMeansPanel {
        def getSrc = harrisFiltered

        lazy val getInitialLabels: Var[Seq[Set[Point]]] = Var(Nil)

        override def drawClusterCenters() = {
          if (repaint_?.get) HarrisPanel.drawHarris()
          super.drawClusterCenters()
        }


        UpperPanel.onError +:= ((_: Throwable) => useInitialLabels.set(false))
      }

      frame.updateForms()
    }
}
