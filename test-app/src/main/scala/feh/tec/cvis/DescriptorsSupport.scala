package feh.tec.cvis

import breeze.stats
import breeze.stats.DescriptiveStats
import feh.dsl.swing2.Var
import feh.tec.cvis.common.AreaDescriptor.{HasStatistics, SingleChannel}
import feh.tec.cvis.common.ChannelDescriptor.Statistics
import feh.tec.cvis.common.cv.Helper.{Array2D, MatWrapper}
import feh.tec.cvis.common.cv.describe.ArgDescriptor
import feh.tec.cvis.common.cv.describe.ArgModifier.MinCap
import feh.tec.cvis.common.{AreaDescriptor, ChannelDescriptor, ImageDescriptor}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import feh.tec.cvis.gui.configurations.GuiArgModifier.Step
import feh.util._
import org.opencv.core._

import scala.swing.event.MouseClicked

trait DescriptorsSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  import DescriptorsSupport._
  
  trait DescriptorsSupportFrame extends ConfigurationsPanelBuilder with MatSupport {
    frame: GenericSimpleAppFrame with FrameExec with LayoutDSL with ConfigBuildHelperGUI =>

    lazy val imageDescriptor: Var[Option[IDescriptor]] = Var(None)

    trait DescriptorsPanel
      extends SimpleVerticalPanel
      with PanelExec[(Mat, Set[Point]), IDescriptor]
      with ConfigBuildHelperPanel
    {
      type Params = (Int, String) // Descriptor side size, image name

      def steps: Int = ???

      def classTag    = scala.reflect.classTag[IDescriptor]
      def getParams() = descriptorSideSize.get -> imageName.get
      def setResult   = Option(_) |> imageDescriptor.set


      lazy val descriptorSideSize     = Var(1)
      lazy val imageName: Var[String] = Var(null)

      
      
      
      object DescriptorSideSize extends ArgDescriptor[Int]("Descriptor side", null, MinCap(1), Step(2))

      lazy val descriptorSideSizeControl = mkNumericControl(DescriptorSideSize)(descriptorSideSize.get, descriptorSideSize.set)

      lazy val imageNameControl = controlFor(imageName.get)(imageName.set).textForm
      
      lazy val descriptorGroupsInfo = monitorFor(imageDescriptor.get.map(_.pointsGroups) getOrElse Map()) // todo: rewrite
                                        .list
                                        .affect(l => l.listenTo(l.mouse.clicks))
                                        .affect(l => l.reactions +=  {
                                                                        case MouseClicked(`l`, p, _, 2, _) =>
                                                                          val ind = l.peer.locationToIndex(p)
                                                                          showGroupInfo( l.peer.getModel.getElementAt(ind) )
                                                                      })
        //Control(imageDescriptor, new Label)

      def showGroupInfo(group: (Double, Set[Point]))

      def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "descriptorSideSize"    -> descriptorSideSizeControl
      , "imageName"             -> (imageNameControl -> label("Image name"))
      , "descriptorGroupsInfo"  -> (descriptorGroupsInfo -> label("Descriptor Groups"))
      )


      type ADescriptor = AreaDescriptor with SingleChannel with HasStatistics{
        type Channel = ChannelDescriptor with Statistics
      }

      def mkDescriptor(img: Mat, sideSize: Int)(p: Point): ADescriptor =
        new AreaDescriptor with SingleChannel with HasStatistics{
          type Channel = ChannelDescriptor with Statistics

          def sideLength = sideSize

          lazy val channel: Channel = new ChannelDescriptor with Statistics{
            private val n = (sideSize - 1).ensuring(_ % 2 == 0, "sideSize must be odd") / 2

            lazy val subMat = img.submat(p.y.toInt-n, p.y.toInt+n, p.x.toInt-n, p.x.toInt+n)

            lazy val data: Array[Double] = subMat.toArray
            lazy val byRows: Array2D[Double] = subMat    .byRow(_ => _.toArray[Double]).toArray
            lazy val byCols: Array2D[Double] = subMat.t().byRow(_ => _.toArray[Double]).toArray


            lazy val mean   = stats.mean(data)
            lazy val std    = stats.stddev(data)
            lazy val range  = data.max - data.min
            lazy val iqr    = DescriptiveStats.percentile(data, 0.75) - DescriptiveStats.percentile(data, 0.25)
          }

  //        lazy val (mean, std) = {
  //          val m = new MatOfDouble
  //          val s = new MatOfDouble
  //          Core.meanStdDev(subMat, m, s)
  //          m.toArray -> s.toArray
  //        }

        }

      def groupDescription(pts: Map[Point, ADescriptor]): Map[Double, Set[Point]] = ???

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      def runner: Runner[Params, (Mat, Set[Point]), IDescriptor] = Runner(
      nextStep => {
        case (sideSize, imgName) =>
        {
          case (img, iPoints) =>
            val pts = iPoints.zipMap(mkDescriptor(img /* todo: normalize */, sideSize)).toMap

            new ImageDescriptor with ImageDescriptor.BetterSearch {
              type ADescriptor = AreaDescriptor with SingleChannel with HasStatistics{
                                    type Channel = ChannelDescriptor with Statistics
                                  }

              def name = imgName

              lazy val originalImage: Array[Byte] = toMat(frame.originalImage).convert(CvType.CV_8U).toArray

              def interestPoints: Map[Point, ADescriptor] = pts

              lazy val pointsGroups: Map[PointsGroupDescriptor, Set[Point]] = groupDescription(pts)
            } : IDescriptor
        }
      }
      )
    }

  }
}

object DescriptorsSupport{
  type IDescriptor = ImageDescriptor with ImageDescriptor.BetterSearch {
                        type ADescriptor = AreaDescriptor with SingleChannel with HasStatistics {
                                              type Channel = ChannelDescriptor with Statistics
                                            }
                      }
}
