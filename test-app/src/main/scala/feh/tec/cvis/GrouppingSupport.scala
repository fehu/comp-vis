package feh.tec.cvis

import java.awt.Color

import feh.tec.cvis.common.Drawing._
import feh.tec.cvis.common.Helper.PointNumericImplicits._
import feh.tec.cvis.common.Helper._
import feh.tec.cvis.common.describe.{ArgDescriptor, ArgModifier}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import feh.util._
import org.opencv.core.Point

import scala.collection.mutable

trait GrouppingSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  trait GrouppingSupportFrame extends ConfigurationsPanelBuilder {
    frame: GenericSimpleAppFrame with FrameExec with LayoutDSL with ConfigBuildHelperGUI =>

    protected var groupsCentersWithPoints = List.empty[(Point, Set[Point])]
    
    trait GroupingPanel
      extends SimpleVerticalPanel
      with PanelExec[List[((Int, Int), Double)], List[(Point, Set[Point])]]
      with ConfigBuildHelperPanel
    {
      type Params = Double // max in-cluster distance

      def steps = 1

      def getParams() = maxPairToPairInClusterDistance

      def setResult = v => {
        groupsCentersWithPoints = v
        drawGroupsCenters()
      }
      def classTag = scala.reflect.classTag[List[(Point, Set[Point])]]



      object MaxPairToPairInClusterDistance extends ArgDescriptor[Double]("Max pair-to-pair in-cluster distance", null, ArgModifier.Positive)

      protected var maxPairToPairInClusterDistance = 10d

      lazy val maxPairToPairInClusterDistanceControl =
        mkNumericControl(MaxPairToPairInClusterDistance)(maxPairToPairInClusterDistance, maxPairToPairInClusterDistance = _) |> fixPreferredSize

      def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "maxPairToPairInClusterDistance" -> maxPairToPairInClusterDistanceControl
      )

      protected def throwIfInterrupted() = if(interrupted_?) throw Interrupted

      def runner = Runner{
                           nextStep =>
                             maxDist =>
                               hFiltered =>
                                 val centers = hFiltered.map(_._1: Point)
                                 val neighbouringMap =(
                                                        for{
                                                          point   <- centers
                                                          another <- centers
                                                          if another != point
                                                          dist = point.distance[EuclideanDistance](another)
                                                          if dist <= maxDist
                                                        } yield point -> another
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
        affectImageMat(img => groupsCentersWithPoints.foreach(p => img.draw.circle(p._1.swap, 5, Color.green)))
        repaintImage()
      }

    }

  }
}
