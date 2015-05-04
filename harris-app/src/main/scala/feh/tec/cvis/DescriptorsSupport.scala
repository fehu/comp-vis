package feh.tec.cvis

import java.text.DecimalFormat
import java.util.UUID
import javax.swing.SwingConstants
import javax.swing.table.{DefaultTableCellRenderer, DefaultTableModel}

import breeze.stats
import breeze.stats.DescriptiveStats
import feh.dsl.swing2.{Monitor, Var}
import feh.tec.cvis.common.AreaDescriptor.{HasStatistics, SingleChannel}
import feh.tec.cvis.common.ChannelDescriptor.Statistics
import feh.tec.cvis.common.cv.Helper._
import feh.tec.cvis.common.cv.describe.ArgDescriptor
import feh.tec.cvis.common.cv.describe.ArgModifier.MinCap
import feh.tec.cvis.common.{AreaDescriptor, ChannelDescriptor, ImageDescriptor}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import feh.tec.cvis.gui.configurations.GuiArgModifier.Step
import feh.util._
import org.opencv.core._

import scala.swing.{Component, ScrollPane, Table}


trait DescriptorsSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  import DescriptorsSupport._
  
  trait DescriptorsSupportFrame extends ConfigurationsPanelBuilder with MatSupport {
    frame: GenericSimpleAppFrame with FrameExec with LayoutDSL with ConfigBuildHelperGUI =>

    lazy val imageDescriptors: Var[Map[Point, ADescriptor]] = Var(Map())

    trait DescriptorsPanel
      extends SimpleVerticalPanel
      with PanelExec[(Mat, Set[Point]), Seq[(Point, ADescriptor)]]
      with ConfigBuildHelperPanel
    {
      type Params = (Int, String) // Descriptor side size, image name

      def steps = 1

      def classTag    = scala.reflect.classTag[Seq[(Point, ADescriptor)]]
      def getParams() = descriptorSideSize.get -> imageName.get
      def setResult   = imageDescriptors set _.toMap


      lazy val descriptorSideSize     = Var(1)
      lazy val imageName: Var[String] = Var(null)

      
      
      
      object DescriptorSideSize extends ArgDescriptor[Int]("Descriptor side", null, MinCap(1), Step(2))

      lazy val descriptorSideSizeControl = mkNumericControl(DescriptorSideSize)(descriptorSideSize.get, descriptorSideSize.set)

//      lazy val imageNameControl = controlFor(imageName.get)(imageName.set).textForm
      
//      lazy val descriptorGroupsInfo = monitorFor(imageDescriptor.get.map(_.interestPoints) getOrElse Map()) // todo: rewrite
//                                        .list
//                                        .affect(l => l.listenTo(l.mouse.clicks))
//                                        .affect(l => l.reactions +=  {
//                                                                        case MouseClicked(`l`, p, _, 2, _) =>
//                                                                          val ind = l.peer.locationToIndex(p)
//                                                                          showPointInfo( l.peer.getModel.getElementAt(ind) )
//                                                                      })
      private def descriptorGroupsInfoModel(data: Seq[(Point, ADescriptor)]) ={
        val names = "Point" :: "Mean" :: "StDev" :: "Range" :: "IQR" :: Nil
        val dArr = data.toArray.map{
          case (p, descr) => Array[AnyRef](p.pairInt,
                                           Double.box(descr.channel.mean),
                                           Double.box(descr.channel.std),
                                           Double.box(descr.channel.range),
                                           Double.box(descr.channel.iqr))
        }
        new DefaultTableModel(dArr, names.toArray[AnyRef]){
          override def isCellEditable(row: Int, column: Int) = false
        }
      }

      lazy val descriptorGroupsInfo = Monitor.custom(imageDescriptors, new Table){
        c =>
          c.model = descriptorGroupsInfoModel(Nil)

          lazy val formatter = new DecimalFormat("0.###E0")

          c.peer.setDefaultRenderer(classOf[java.lang.Double], new DefaultTableCellRenderer.UIResource{
            setHorizontalAlignment(SwingConstants.RIGHT)
            override def setValue(value: AnyRef): Unit = setText(Option(value) map formatter.format getOrElse "")
          })
      }{
        c =>
          t =>
            c.model = descriptorGroupsInfoModel(t.toSeq)
      }

//      def showPointInfo(group: (Point, ADescriptor))

      lazy val formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "descriptorSideSize"    -> descriptorSideSizeControl
//      , "imageName"             -> (imageNameControl -> label("Image name"))
//      , "descriptorGroupsInfo"  -> (descriptorGroupsInfo.component -> label("Descriptor Groups"))
      )


      override lazy val elems: Seq[(String, Seq[Component])] = mkElems ++ Seq(
        "descriptorGroupsInfo" -> Seq(new ScrollPane(descriptorGroupsInfo.component))
      )

      // todo: bounding conditions!
      def mkDescriptor(img: Mat, sideSize: Int)(p: Point) = {
        val n = (sideSize - 1).ensuring(_ % 2 == 0, "sideSize must be odd") / 2

        val subMat =  if(n > 1) img.submat(p.x.toInt-n, p.x.toInt+n, p.y.toInt-n, p.y.toInt+n)
                      else      new MatOfDouble(img.get(p.x.toInt, p.y.toInt): _*)

        val data: Array[Double] = subMat.toArray

        ADescriptor( sideSize
                   , subMat.toArray
                   , stats.mean(data)
                   , stats.stddev(data)
                   , data.max - data.min
                   , DescriptiveStats.percentile(data, 0.75) - DescriptiveStats.percentile(data, 0.25)
                   )
        }

      def groupDescription(pts: Map[Point, ADescriptor]): Map[Double, Set[Point]] = ???

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      def runner: Runner[Params, (Mat, Set[Point]), Seq[(Point, ADescriptor)]] = Runner(
      nextStep => {
        case (sideSize, imgName) =>
        {
          case (img, iPoints) => iPoints.toSeq.zipMap(mkDescriptor(img.convert(CvType.CV_64F).normalize, sideSize))
        }
      }
      )
    }

  }
}

object DescriptorsSupport{

//  type IDescriptor = ImageDescriptor {
//    type ADescriptor = DescriptorsSupport.ADescriptor
//  }

  case class IDescriptor( name: String
                        , sideLength: Int
                        , originalImage: Array[Byte]
                        , interestPoints: Map[Point, ADescriptor]
                        )
                        (val id: Option[UUID] = None) extends ImageDescriptor
  {
    type ADescriptor        = DescriptorsSupport.ADescriptor
    def descriptorChannels  = 1
  }

  case class ADescriptor( sideLength: Int
                        , data      : Array[Double]
                        , mean      : Double
                        , std       : Double
                        , range     : Double
                        , iqr       : Double
                        )
    extends AreaDescriptor with SingleChannel with HasStatistics
  {

    type Channel = ChannelDescriptor with Statistics

    lazy val channel: Channel = new ChannelDescriptor with Statistics{
      def data = ADescriptor.this.data

      def n = sideLength*2+1
      def sideRange = 0 until n

      lazy val byRows: Array2D[Double] = (
        for(i <- sideRange) yield (
          for(j <- sideRange) yield data(i*n + j)
          ).toArray
        ).toArray

      lazy val byCols: Array2D[Double] = (
        for(j <- sideRange) yield (
          for(i <- sideRange) yield data(i*n + j)
          ).toArray
        ).toArray

      def mean  = ADescriptor.this.mean
      def std   = ADescriptor.this.std
      def range = ADescriptor.this.range
      def iqr   = ADescriptor.this.iqr
    }
  }
}
