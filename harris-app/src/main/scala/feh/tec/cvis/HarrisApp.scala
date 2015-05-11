package feh.tec.cvis

import java.awt.image._
import java.awt.{Color, Dimension}
import java.nio.ByteBuffer
import java.util.UUID

import feh.dsl.swing2.Var
import feh.tec.cvis.DescriptorsSupport.{ADescriptor, IDescriptor}
import feh.tec.cvis.common.cv.Helper._
import feh.tec.cvis.common.cv.describe.{CallHistoryContainer, CallHistory}
import feh.tec.cvis.common.cv.{CV, CornerDetection, Drawing}
import feh.tec.cvis.db.{HasDescriptorCache, HasDbConnections}
import feh.tec.cvis.db.SingleChannelDescriptorsWithStats._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import org.opencv.core._
import slick.driver.H2Driver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
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
      with HistorySupport
      with HarrisSupportFrame
      with KMeansSupportFrame
      with GroupingSupportFrame
      with DescriptorsSupportFrame
      with AdminSupportFrame
      with UserSupportFrame
      with CornerDetection
      with MatSupport
      with HasDbConnections
      with HasDescriptorCache
    {
      frame =>

      def dbAccessTimeout: FiniteDuration = 200.millis

//      implicit val db = DbConnection(Database.forConfig("h2harris"))
      implicit val db = DbConnection(Database.forConfig("h2harrisDev")); println("using `dev` db")

      override def stop(): Unit = {
        db.close(dbAccessTimeout)
        super.stop()
      }

      db.tryCreateTables( (table.imageDescriptors.schema ++ table.pointDescriptors.schema).create )

//      LayoutDebug = true

      protected val distinctInterestPoints: Var[CallHistoryContainer[Set[(Int, Int)]]] =
        Var(CallHistoryContainer.empty(Set()))

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

        def getSrc = harrisFiltered.get

        override def drawGroupsCenters(): Unit = {
          KMeansPanel.getInitialLabels set groupsCentersWithPoints.get.value.map(_._2) // todo: should be in another place, not in draw

          if(repaint_?.get) HarrisPanel.drawHarris()
          super.drawGroupsCenters()
        }
      }



      object KMeansPanel extends KMeansPanel {
        def getSrc = harrisFiltered.get

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

        groupsCentersWithPoints.onChange(l => if(l.value.isEmpty) sourcesAvailable.affect(_ - Source.Grouping)
                                              else                sourcesAvailable.affect(_ + Source.Grouping))

        clusteringResult       .onChange(l => if(l.value.isEmpty) sourcesAvailable.affect(_ - Source.KMeans)
                                              else                sourcesAvailable.affect(_ + Source.KMeans))

        sourcesAvailable.onChange(_ => sourceControl.component.tryUpdate())

        lazy val sourceControl = controlForSeq(sourcesAvailable.get.toSeq).dropDownList(gcSrc.set)


        override def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
          "distinctSourceControl"           -> (sourceControl -> label("Source"))
        , "maxPairToPairInClusterDistance2" -> maxPairToPairInClusterDistanceControl
        )

        def getSrc: CallHistoryContainer[List[Point]] = gcSrc.get match {
          case Source.Grouping  => groupsCentersWithPoints.get.affect(CallHistory.Entry("take group center"))(_.map(_._1))
          case Source.KMeans    => clusteringResult.get.affect(CallHistory.Entry("take cluster centers"))(_.centers)
          case Source.None      => CallHistoryContainer.empty(Nil)
        }


        override def setResult: CallHistoryContainer[List[(Point, Set[Point])]] => Unit = {
          res =>
            distinctInterestPoints set res.affect(CallHistory.Entry("points to (Int, Int)"))(_.map(_._1.pairInt).toSet)
            drawGroupsCenters()
          }

        override def drawGroupsCenters(): Unit = {
          if(repaint_?.get) HarrisPanel.drawHarris()

          affectImageMat(img => distinctInterestPoints.get.value.foreach{
             p => img.draw.circle(p.swap, maxPairToPairInClusterDistance.toInt, Color.cyan, thickness = 2)
           })
          repaintImage()
        }
      }


      object DescriptorsPanel extends DescriptorsPanel{
        def getSrc: CallHistoryContainer[(Mat, Set[Point])] = distinctInterestPoints.get
                .affect(CallHistory.Entry("mk points from pairs"))(pts => originalGray -> pts.map(x => x: Point))
      }


      object AdminPanel extends AdminPanel{

        def dbAccessTimeout = frame.dbAccessTimeout

        def getSrc: (Int, Int, Int, Int, Array[Byte], CallHistoryContainer[Map[Point, ADescriptor]]) =
          ( originalMat.width()
          , originalMat.height()
          , originalMat.`type`()
          , originalImage.getType
          , originalMat.toArray[Byte]
          , imageDescriptors.get
          )

        showHistoryFrameTrigger.component.enabled = false
        imageDescriptors.onChange{ h => showHistoryFrameTrigger.enabled = h.value.nonEmpty }

        def fetchDbInfo(): Future[Seq[(String, Int)]] = db.run(query.namesAndCounts)

        def setResult: (IDescriptor) => Unit = d => db.run(query.insert(d))
      }


      object UserPanel extends UserPanel{
        def getSrc: CallHistoryContainer[Map[Point, ADescriptor]] = imageDescriptors.get


        def fetchDescriptor(id: UUID)= Await.result(descriptorCache.get(id), frame.dbAccessTimeout)

        def searchDbTimeout = frame.dbAccessTimeout

        def searchDb(mean: Option[Double], std: Option[Double], range: Option[Double], iqr: Option[Double], precision: Double): Future[Map[(UUID, String), Seq[(Int, Int)]]] =
          query.searchBy(mean, std, range, iqr, precision) map db.run getOrElse Future{ Map() }
      }


      frame.updateForms()
    }
}
