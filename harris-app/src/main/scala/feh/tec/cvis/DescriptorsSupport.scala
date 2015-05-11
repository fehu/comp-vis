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
import feh.tec.cvis.common.cv.describe.CallHistory.ArgEntry
import feh.tec.cvis.common.cv.describe.{CallHistory, CallDescriptor, CallHistoryContainer, ArgDescriptor}
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
    frame: GenericSimpleAppFrame
            with FrameExec
            with LayoutDSL
            with HistorySupport
            with ConfigBuildHelperGUI =>

    lazy val imageDescriptors: Var[CallHistoryContainer[Map[Point, ADescriptor]]] = Var(CallHistoryContainer.empty(Map()))

    trait DescriptorsPanel
      extends SimpleVerticalPanel
      with PanelExecHistory[(Mat, Set[Point]), Seq[(Point, ADescriptor)]]
      with ConfigBuildHelperPanel
    {
      def steps = 1

      def classTag    = scala.reflect.classTag[Seq[(Point, ADescriptor)]]

      def params: Set[ArgEntry[_]] = Set( ArgEntry(DescriptorSideSize, descriptorSideSize.get) )

      def callDescriptor: CallDescriptor[Seq[(Point, ADescriptor)]] = CallDescriptor("descriptors")

      def setResult: (CallHistoryContainer[Seq[(Point, ADescriptor)]]) => Unit =
        imageDescriptors set _.affect(CallHistory.Entry("toMap"))(_.toMap)



      lazy val descriptorSideSize     = Var(1)

      object DescriptorSideSize extends ArgDescriptor[Int]("Descriptor side", null, MinCap(1), Step(2))

      lazy val descriptorSideSizeControl = mkNumericControl(DescriptorSideSize)(descriptorSideSize.get, descriptorSideSize.set)

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
            c.model = descriptorGroupsInfoModel(t.value.toSeq)
      }

      lazy val formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "descriptorSideSize" -> descriptorSideSizeControl
      )

      override lazy val elems: Seq[(String, Seq[Component])] = mkElems ++ Seq(
        "descriptorGroupsInfo" -> Seq(new ScrollPane(descriptorGroupsInfo.component))
      )

      // todo: bounding conditions!
      def mkDescriptor(img: Mat, sideSize: Int)(p: Point) = {
        val n = (sideSize - 1).ensuring(_ % 2 == 0, "sideSize must be odd") / 2

        val subMatOpt = if(n > 1)
                          if(p.x + n  > img.width || p.x - n  < 0 || p.y + n  > img.height || p.y - n  < 0) None
                          else Some(img.submat(p.x.toInt-n, p.x.toInt+n, p.y.toInt-n, p.y.toInt+n))
                        else Some(new MatOfDouble(img.get(p.x.toInt, p.y.toInt): _*))

        val data: Array[Double] = subMatOpt.map(_.toArray[Double]) getOrElse Array.empty

        if(data.nonEmpty) Some( ADescriptor( sideSize
                                           , data
                                           , stats.mean(data)
                                           , stats.stddev(data)
                                           , data.max - data.min
                                           , DescriptiveStats.percentile(data, 0.75) - DescriptiveStats.percentile(data, 0.25)
                                           ))
        else None
      }

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      lazy val runner: Runner[Params, (Mat, Set[Point]), Seq[(Point, ADescriptor)]] = Runner(
      nextStep =>
        params =>
        {
          case (img, iPoints) => iPoints.toSeq
                                  .zipMap(mkDescriptor(img.convert(CvType.CV_64F).normalize /* todo: normalize? */, params.arg(DescriptorSideSize)))
                                  .filter(_._2.isDefined)
                                  .mapVals(_.get)
        }
      )
    }

  }
}

object DescriptorsSupport{


  case class IDescriptor( name: String
                        , sideLength: Int
                        , matType: Int
                        , javaType: Int
                        , originalSize: Size
                        , originalImage: Array[Byte]
                        , interestPoints: Map[Point, ADescriptor]
                        , interestPointsHistory: CallHistory[Map[Point, ADescriptor]]
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
