package feh.tec.cvis

import java.awt.Color

import feh.dsl.swing.util.AwtUtils
import feh.tec.cvis.common.describe.{ArgModifier, ArgDescriptor}
import feh.tec.cvis.common.describe.ArgModifier.MinCap
import feh.tec.cvis.common.{Drawing, TerminationCriteria, Clustering}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import feh.util._
import feh.tec.cvis.common.Helper._
import org.opencv.core.{Mat, CvType}
import scala.swing.Dialog
import scala.swing.Swing._
import Drawing._


trait KMeansSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  trait KMeansSupportFrame extends ConfigurationsPanelBuilder with Clustering with AwtUtils{
    frame: GenericSimpleAppFrame with FrameExec with LayoutDSL with ConfigBuildHelperGUI =>

    protected var clusteringResult = KMeansResult.empty
    protected var initialNClusters: Int


    trait KMeansPanel
      extends SimpleVerticalPanel
      with PanelExec[List[((Int, Int), Double)], KMeansResult]
      with ConfigBuildHelperPanel
    {

      def steps = nClustersMaxTries

      // get / set

      final type Params = Unit
      final def getParams() = ()

      final def classTag = scala.reflect.classTag[KMeansResult]

      def setResult: (KMeansResult) => Unit = {
        res =>
          clusteringResult = res
          drawClusterCenters()
      }


      // configurable vars

      var nClustersStep = 10
      var nClustersMaxTries = 100

      def tpe: TerminationCriteria.Type = TerminationCriteria.Type.Both // todo both for now
      var criteriaMaxCount: Int = 1000
      var criteriaEpsilon: Double = 1e-3

      var attempts = 100
      var centersInitialPolicy: CentersPolicy = CentersPolicy.Random

      var targetCompactness = 1e4

      def criteria = TerminationCriteria(criteriaMaxCount, criteriaEpsilon)

      object InitialNClusters     extends ArgDescriptor[Int]("initial number of clusters",              null, MinCap(1))
      object NClustersStep        extends ArgDescriptor[Int]("step for the number of clusters search",  null, MinCap(1))
      object NClustersMaxTries    extends ArgDescriptor[Int]("step for the number of clusters search",  null, MinCap(1))

      object CriteriaMaxCount     extends ArgDescriptor[Int]    ("maximum tries", null, MinCap(1))
      object CriteriaEpsilon      extends ArgDescriptor[Double] ("epsilon",       null, ArgModifier.Positive)

      object Attempts             extends ArgDescriptor[Int]          ("Number of attempts",     null, MinCap(1))

      object CentersInitialPolicy extends ArgDescriptor[CentersPolicy]("centers initial policy", null)
      lazy val centersInitialPolicyDomain = CentersPolicy.Random :: CentersPolicy.PP :: Nil

      object TargetCompactness    extends ArgDescriptor[Double]("Target clusters compactness", null, ArgModifier.Positive)

      // configurable vars controls

      lazy val initialNClustersControl      = mkNumericControl(InitialNClusters)  (initialNClusters, initialNClusters = _)
      lazy val nClustersStepControl         = mkNumericControl(NClustersStep)     (nClustersStep, nClustersStep = _)
      lazy val nClustersMaxTriesControl     = mkNumericControl(NClustersMaxTries) (nClustersMaxTries, nClustersMaxTries = _)

      lazy val attemptsControl              = mkNumericControl(Attempts)          (attempts, attempts = _)
      lazy val targetCompactnessControl     = mkNumericControl(TargetCompactness) (targetCompactness, targetCompactness = _) |> fixPreferredSize
      lazy val centersInitialPolicyControl  = mkListControl(CentersInitialPolicy, centersInitialPolicyDomain)(centersInitialPolicy = _, _.toString)

      lazy val criteriaMaxCountControl      = mkNumericControl(CriteriaMaxCount)  (criteriaMaxCount, criteriaMaxCount = _)
      lazy val criteriaEpsilonControl       = mkNumericControl(CriteriaEpsilon)   (criteriaEpsilon, criteriaEpsilon = _) |> fixPreferredSize

      lazy val criteriaControlPanel = panel.grid(1, 1)(
        mkSmallPanel("criteriaMaxCount")(Seq(criteriaMaxCountControl._2, criteriaMaxCountControl._1))                   -> "criteriaMaxCount"
      , (mkSmallPanel("criteriaEpsilon")(Seq(criteriaEpsilonControl._2, criteriaEpsilonControl._1))) -> "criteriaEpsilon"
      )


      def fixPreferredSize[B <: AbstractDSLBuilder]: ((B, DSLLabelBuilder[_])) => (B, DSLLabelBuilder[_]) = {
        case (c, l) => c.affect(x => x.preferredSize = 200 -> x.preferredSize._2).asInstanceOf[B] -> l
      }

      lazy val formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
          "initialNClusters"    -> initialNClustersControl
        , "nClustersStep"       -> nClustersStepControl
        , "nClustersMaxTries"   -> nClustersMaxTriesControl
        , "criteriaPanel"       -> (criteriaControlPanel -> label("Termination criteria"))
        , "attempts"            -> attemptsControl
        , "centersInitialPolicy"-> centersInitialPolicyControl
        , "targetCompactness"   -> targetCompactnessControl
      )

      // running

      lazy val runner = RecursiveRunner[Int, Unit, List[((Int, Int), Double)], KMeansResult]{
        _ => iPoints =>
          if(iPoints.length >= initialNClusters){
            val cData = iPoints.toMatOfPoint.convert(CvType.CV_32F).t()

            val best = new Mat()
            println("new best")
            def centersPolicy = if(best.empty()) centersInitialPolicy
            else CentersPolicy.InitialLabels
            def kMeans(nClusters: Int) = {
              println("kMeans")
              kmeans(cData, nClusters, criteria, attempts, centersPolicy, best)
            }

            nClust =>
              kMeans(nClust) |> {
                res =>
                  println(s"kmeans with $nClust clusters: compactness = ${res.compactness}")

                  UpperPanel updateProgress s"$nClust clusters: compactness = ${res.compactness}"

                  if(res.compactness <= targetCompactness)  scala.Right(res)
                  else                                      scala.Left(nClust+nClustersStep)
              }
          }
          else {
            Dialog.showMessage(parent = frame,
                               message = "Number of clusters shouldn't be greater than number of points",
                               title = "Warning",
                               messageType = Dialog.Message.Warning)
            nClust => scala.Right(KMeansResult.empty)
          }
      }.runner( initial = initialNClusters
              , maxTries = nClustersMaxTries
              , err = s"Couldn't reach targetCompactness $targetCompactness in $nClustersMaxTries tries")


      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted


      // draw

      def drawClusterCenters()  = {
        affectImageMat(img => clusteringResult.centers.foreach(p => img.draw.circle(p.swap, 5, Color.blue)))
        repaintImage()
      }

    }

  }

}
