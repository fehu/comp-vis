package feh.tec.cvis

import java.awt.image._
import java.awt.{Color, Dimension}
import java.nio.ByteBuffer
import java.util.UUID

import feh.dsl.swing2.Var
import feh.tec.cvis.DescriptorsSupport.{ADescriptor, IDescriptor}
import feh.tec.cvis.common.cv.Helper._
import feh.tec.cvis.common.cv.{CV, CornerDetection, Drawing}
import feh.tec.cvis.db.{DescriptorCacheScope, HasDbConnections}
import feh.tec.cvis.db.SingleChannelDescriptorsWithStats._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import org.opencv.core._
import slick.driver.H2Driver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.swing.Swing._

object HarrisApp extends DefaultApp("Harris interest points", 300 -> 300, 600 -> 800)
  with HarrisSupport 
  with KMeansSupport
  with GroupingSupport
  with DescriptorsSupport
  with AdminSupport
  with UserSupport
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
      with AdminSupportFrame
      with UserSupportFrame
      with CornerDetection
      with MatSupport
      with HasDbConnections
    {
      frame =>

      def dbAccessTimeout: FiniteDuration = 200.millis

      val db = DbConnection(Database.forConfig("h2harris"))

      override def stop(): Unit = {
        db.close(dbAccessTimeout)
        super.stop()
      }

      db.tryCreateTables( (table.imageDescriptors.schema ++ table.pointDescriptors.schema).create )

//      LayoutDebug = true

      protected val distinctInterestPoints: Var[Set[(Int, Int)]] = Var(Set())

      type Config = SimpleVerticalPanel with PanelExec[_, _]


      lazy val configurations: Seq[(String, Config)] = Seq(
          "harris"      -> HarrisPanel
        , "grouping"    -> GroupingPanel
        , "k-means"     -> KMeansPanel
        , "distinct"    -> DistinctPanel
        , "describe"    -> DescriptorsPanel
        , "admin"       -> AdminPanel
        , "user"        -> UserPanel
      )



      object GroupingPanel extends GroupingPanel{

        def getSrc = harrisFiltered.map(_._1: Point)

        override def drawGroupsCenters(): Unit = {
          KMeansPanel.getInitialLabels set groupsCentersWithPoints.get.map(_._2) // todo: should be in another place, not in draw

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
      }


      object AdminPanel extends AdminPanel{

        def dbAccessTimeout = frame.dbAccessTimeout

        def getSrc: (Int, Int, Int, Int, Array[Byte], Map[Point, ADescriptor]) = {
          val iarr = originalMat.convert(CvType.CV_32S).toArray[Int]
          val barr = Array.ofDim[Byte](iarr.length*4)
          val buff = ByteBuffer.wrap(barr)
          for(i <- iarr) buff.putInt(i)

          ( originalMat.width()
          , originalMat.height()
          , originalMat.`type`()
          , originalImage.getType
          , barr
          , imageDescriptors.get
          )
        }

        def fetchDbInfo(): Future[Seq[(String, Int)]] = db.run(query.namesAndCounts)

        def setResult: (IDescriptor) => Unit = d => db.run(query.insert(d))
      }


      object UserPanel extends UserPanel{
        def getSrc: Map[Point, ADescriptor] = imageDescriptors.get


        def searchDbTimeout = frame.dbAccessTimeout

        def searchDb(mean: Option[Double], std: Option[Double], range: Option[Double], iqr: Option[Double], precision: Double): Future[Map[(UUID, String), Seq[(Int, Int)]]] =
          query.searchBy(mean, std, range, iqr, precision) map db.run getOrElse Future{ Map() }
      }


      frame.updateForms()
    }
}
