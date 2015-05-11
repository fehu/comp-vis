package feh.tec.cvis

import java.awt.Color

import feh.dsl.swing2.Var
import feh.tec.cvis.common.cv.describe.CallHistory.ArgEntry
import feh.tec.cvis.common.cv.{Helper, Drawing}
import Drawing._
import Helper.PointNumericImplicits._
import Helper._
import feh.tec.cvis.common.cv.describe._
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import feh.util._
import org.opencv.core.Point

import scala.collection.mutable

trait GroupingSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  trait GroupingSupportFrame extends ConfigurationsPanelBuilder {
    frame: GenericSimpleAppFrame
            with FrameExec
            with LayoutDSL
            with HistorySupport
            with ConfigBuildHelperGUI =>

    protected lazy val groupsCentersWithPoints: Var[CallHistoryContainer[List[(Point, Set[Point])]]] =
      Var(CallHistoryContainer.empty(Nil))

    trait GroupingPanel
      extends SimpleVerticalPanel
      with PanelExecHistory[List[Point], List[(Point, Set[Point])]]
      with ConfigBuildHelperPanel
    {
      def steps = 1

      def setResult = v => {
        groupsCentersWithPoints set v
        drawGroupsCenters()
      }

      def classTag = scala.reflect.classTag[List[(Point, Set[Point])]]

      def callDescriptor = GroupingDescriptor

      def params: Set[ArgEntry[_]] = Set(
        ArgEntry(MaxPairToPairInClusterDistance, maxPairToPairInClusterDistance)
      )

      object GroupingDescriptor extends CallDescriptor[List[(Point, Set[Point])]]{ def name = "Grouping" }


      object MaxPairToPairInClusterDistance extends ArgDescriptor[Double]("Max pair-to-pair in-cluster distance", null, ArgModifier.Positive)

      protected var maxPairToPairInClusterDistance = 10d

      lazy val maxPairToPairInClusterDistanceControl =
        mkNumericControl(MaxPairToPairInClusterDistance)(maxPairToPairInClusterDistance, maxPairToPairInClusterDistance = _) |> fixPreferredSize

      def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "maxPairToPairInClusterDistance" -> maxPairToPairInClusterDistanceControl
      )

      protected def throwIfInterrupted() = if(interrupted_?) throw Interrupted

      lazy val runner = Runner{
                           nextStep =>
                             maxDist: Params =>
                               centers: List[Point] =>

                                 val neighbouringMap=(
                                                       centers flatMap {
                                                         point =>
                                                           val pts = for{
                                                             another <- centers
                                                             if another != point
                                                             dist = point.distance[EuclideanDistance](another)
                                                             if dist <= maxDist.arg(MaxPairToPairInClusterDistance) * 2
                                                           } yield point -> another

                                                           if(pts.isEmpty) List(point -> point) else pts
                                                     }
                                                     ).groupBy(_._1)
                                                      .mapValues(_.map(_._2))

                                 val g = mutable.Buffer.empty[mutable.HashSet[Point]]

                                 for ((point, neighbours) <- neighbouringMap)
                                   g.find(_ contains point)
                                   .map(_ ++= neighbours)
                                   .getOrElse(g += (new mutable.HashSet += point ++= neighbours))

                                 val cCenters =  for {
                                   grouped <- g
                                   n = grouped.size
                                   center = grouped.sum / (n -> n)
                                 } yield center -> grouped.toSet

                                 cCenters.toList
                         }

      def drawGroupsCenters()  = {
        affectImageMat(img => groupsCentersWithPoints.get.value.foreach{ p => img.draw.circle(p._1.swap, maxPairToPairInClusterDistance.toInt, Color.green) })
        repaintImage()
      }

    }

  }
}
