package feh.tec.cvis.testapp

import java.awt.image._
import java.awt.{Color, Dimension}

import feh.dsl.swing2.Var
import feh.tec.cvis.common.cv.Helper._
import feh.tec.cvis.common.cv.{CV, CornerDetection, Drawing}
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.{DescriptorsSupport, GroupingSupport, HarrisSupport, KMeansSupport}
import org.opencv.core._

import scala.swing.Swing._

object TestHarris extends DefaultApp("harris-test", 300 -> 300, 600 -> 800) 
  with HarrisSupport 
  with KMeansSupport
  with GroupingSupport
  with DescriptorsSupport
  with Drawing
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
      with GroupingSupportFrame
      with DescriptorsSupportFrame
      with CornerDetection
      with MatSupport
    {
      frame =>

//      LayoutDebug = true

      protected val distinctInterestPoints: Var[Set[(Int, Int)]] = Var(Set())

      type Config = SimpleVerticalPanel with PanelExec[_, _]


      lazy val configurations: Seq[(String, Config)] = Seq(
          "harris"      -> HarrisPanel
        , "grouping"    -> GroupingPanel
        , "k-means"     -> KMeansPanel
        , "distinct"    -> DistinctPanel
        , "describe"    -> DescriptorsPanel
      )



      object GroupingPanel extends GroupingPanel{

        def getSrc = harrisFiltered.map(_._1: Point)

        override def drawGroupsCenters(): Unit = {
          KMeansPanel.getInitialLabels set groupsCentersWithPoints.get.map(_._2) // todo: should be in another place, not in draw
          // {
//            val inGroups = groupsCentersWithPoints.get.map(_._2)
//            val outOfGroups = harrisFiltered
//                                .withFilter(x => !inGroups.exists(_.contains(x._1)))
//                                .map{
//                                  case (p, _) => Set(p: Point)
//                                }
//            inGroups ++ outOfGroups
//          }

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



      object DistinctPanel extends GroupingPanel{

        sealed trait Source
        object Source{
          case object None      extends Source{ override def toString = "[None]" }
          case object Grouping  extends Source{ override def toString = "grouping" }
          case object KMeans    extends Source{ override def toString = "k-means" }
        }

        lazy val gcSrc: Var[Source] = Var(Source.None)

        protected lazy val sourcesAvailable = Var(Set[Source](Source.None))

        groupsCentersWithPoints.onChange(l => if(l.isEmpty) sourcesAvailable.affect(_ - Source.Grouping)
                                              else          sourcesAvailable.affect(_ + Source.Grouping))

        clusteringResult       .onChange(l => if(l.isEmpty) sourcesAvailable.affect(_ - Source.KMeans)
                                              else          sourcesAvailable.affect(_ + Source.KMeans))

        sourcesAvailable.onChange(_ => sourceControl.component.tryUpdate())

        lazy val sourceControl = controlForSeq(sourcesAvailable.get.toSeq).dropDownList(gcSrc.set)


        override def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
          "distinctSourceControl"           -> (sourceControl -> label("Source"))
        , "maxPairToPairInClusterDistance2" -> maxPairToPairInClusterDistanceControl
        )


        def getSrc: List[Point] = gcSrc.get match {
          case Source.Grouping  => groupsCentersWithPoints.get.map(_._1)
          case Source.KMeans    => clusteringResult.get.centers
          case Source.None      => Nil
        }

        override def setResult: (List[(Point, Set[Point])]) => Unit = v => {
          distinctInterestPoints set v.map(_._1.pairInt).toSet
          drawGroupsCenters()
        }

        override def drawGroupsCenters(): Unit = {
          if(repaint_?.get) HarrisPanel.drawHarris()

          affectImageMat(img => distinctInterestPoints.get.foreach{ p => img.draw.circle(p.swap, maxPairToPairInClusterDistance.toInt, Color.cyan, thickness = 2) })
          repaintImage()
        }
      }


      object DescriptorsPanel extends DescriptorsPanel{
        def getSrc: (Mat, Set[Point]) = originalGray -> distinctInterestPoints.get.map(x => x: Point)

//        def showPointInfo(group: (Point, ADescriptor)) = Dialog.showMessage(frame, "todo")
      }



      frame.updateForms()
    }
}
