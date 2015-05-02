package feh.tec.cvis.testapp

import java.awt.{Color, Dimension}
import java.awt.image._
import javax.swing.SpinnerNumberModel
import feh.tec.cvis.common.Helper._
import feh.tec.cvis.common._
import feh.tec.cvis.common.describe.ArgModifier.MinCap
import feh.tec.cvis.common.describe._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.GuiArgModifier.Step
import feh.tec.cvis.gui.configurations.Harris
import feh.tec.cvis.gui.configurations.{GuiArgModifier, Harris}
import feh.util._
import org.opencv.core.{CvType, TermCriteria, Core, Mat}
import scala.swing.{Dialog, Component}
import scala.swing.Swing._
import Drawing._

object TestHarris extends DefaultApp("harris-test", 300 -> 300, 600 -> 800) with Harris{

  CV.loadNative()

  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit ) =
    new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
      with ConfigurationsPanelBuilder
      with HarrisGUI
      with FrameExec
      with CornerDetection
      with MatSupport
      with ColorConverting
      with Clustering
    {
      frame =>

//      LayoutDebug = true

      protected def applyMat: Mat = originalMat

      type Config = SimpleVerticalPanel with PanelExec[_, _]

      protected var originalGray: Mat = null

      protected var harrisResult  : List[((Int, Int), Double)] = Nil
      protected var harrisFiltered: List[((Int, Int), Double)] = Nil

      protected var filteredInterestPointsCount: Int = 0
      protected var initialNClusters: Int = 1

      protected var clusteringResult = KMeansResult.empty

      lazy val configurations: Seq[(String, Config)] = Seq(
        "harris"      -> HarrisPanel,
        "clustering"  -> ClusteringPanel
      )

      def calcInitialNClusters(c: Int) =  if      (c > 1000)  c / 100
                                          else if (c > 10)    c / 10
                                          else                1

      object HarrisPanel extends SimpleVerticalPanel with HarrisConfigurationPanelExec{
        panel =>

        def kStep = Some(GuiArgModifier.Step(0.001))

        def getSrc = originalMat
        def setResult: Mat => Unit = _ => drawHarris()

        var threshold: Double = 0.1
        lazy val thresholdControl = controlForOrdered(threshold)(threshold = _)
                                    .spinner(new SpinnerNumberModel(threshold, 0, Double.PositiveInfinity, 0.001))

        lazy val applyThresholdButton = triggerFor{
          setHarrisFiltered( filterHarris(harrisResult) )
          drawHarris()
          frame.updateForms()
        }.button("Apply Threshold")

        var responseFunc: ResponseFunc = ResponseFunc.Original
        lazy val responseFuncControl = controlForSeq(ResponseFunc.all,  static = true)
                                       .dropDownList{
                                                      rf =>
                                                        responseFunc = rf
                                                        componentAccess.get("k").get.visible = rf == ResponseFunc.Original
                                                    }

        case class ResponseFunc(name: String, fromGray: Mat => Stream[((Int, Int), Double)]){
          override def toString = name
        }

        object ResponseFunc{
          def all = Original :: DetTrace :: DetTraceSq :: Nil

          object Original extends ResponseFunc("det - k*trace^2",
                                               grayImg =>
                                                 cornerHarris(grayImg, blockSize, kSize, k.toDouble, Option(borderType))
                                                 .mapV { case Array(d) => d }
                                                 .lazyPairs
          )
          object DetTrace   extends ResponseFunc("det / trace",   withEigenValues(cornerResponseDetTrace))
          object DetTraceSq extends ResponseFunc("det / trace^2", withEigenValues(cornerResponseDetTraceSq))

          def cornerResponseDetTrace:   EigenValsAndVecs => Double    =   eign => eign.det / eign.trace
          def cornerResponseDetTraceSq: EigenValsAndVecs => Double    =   eign => eign.det / math.pow(eign.trace, 2)

          def withEigenValues(response: EigenValsAndVecs => Double) =
            (grayImg: Mat) =>
              cornerEigenValsAndVecs(grayImg, blockSize, kSize, Option(borderType))
              .lazyPairs
              .mapVals(response)
        }

        def setHarrisFiltered(seq: Seq[((Int, Int), Double)]) = {
          harrisFiltered = seq.toList
          filteredInterestPointsCount = harrisFiltered.length
          initialNClusters = calcInitialNClusters(filteredInterestPointsCount)
        }

        override lazy val runner: Runner[Params, Mat, Mat] = Runner {
          params =>
            src =>
              val cvt = ColorConversion(imageColorType, ColorMode.Gray)
              cvtColor(src, cvt) |> {
                grayImg =>
                  originalGray = grayImg.convert(cvt.inverse)

                  val responses = responseFunc.fromGray(grayImg)
                  harrisResult = responses.toList            // TODO !!! no side effects should be present here

                  setHarrisFiltered( filterHarris(harrisResult) )
                grayImg
              }
        }
//        override lazy val runner: Runner[Params, Mat, Mat] = Runner(
//          params =>
//            CallDescriptor.WithScopeAndParams(ConvertColor.Descriptor, frame,
//                                              (ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None)) chain
//            CallDescriptor.WithScopeAndParams(Harris.Descriptor, frame,
//                                              (blockSize, kSize, k.toDouble, Option(borderType))) chain
//            CallDescriptor.Callable()
//        )

        def filterHarris: Seq[((Int, Int), Double)] => Seq[((Int, Int), Double)] = _.filter(_._2 >= threshold)

        lazy val showFilteredInterestPointsCount  = monitorFor(s"Filtered interest points: $filteredInterestPointsCount").text

        override def formBuilders: Seq[(String, (TestHarris.DSLFormBuilder[_], TestHarris.DSLLabelBuilder[_]))] =
          super.formBuilders ++ Seq(
            "responseFunc"                  -> (responseFuncControl -> label("Response Function")),
            "threshold"                     -> (thresholdControl    -> label("Threshold")),
            "applyThreshold"                -> (applyThresholdButton            -> label("")),
            "filteredInterestPointsCount"   -> (showFilteredInterestPointsCount -> label(""))
          )

        def drawHarris() = {
          Option(originalGray) map (_.clone()) foreach setImageMat
          affectImageMat(img => harrisFiltered.foreach{ case ((i, j), r) => img.draw.circle(j -> i, 1, Color.red) })
          repaintImage()
        }
      }

//      List[((Int, Int), Double)]

      object ClusteringPanel
        extends SimpleVerticalPanel
        with PanelExec[List[((Int, Int), Double)], KMeansResult]
        with ConfigBuildHelperPanel
      {

        // get / set

        final type Params = Unit
        final def getParams() = ()

        final def classTag = scala.reflect.classTag[KMeansResult]

        def setResult: (KMeansResult) => Unit = {
          res =>
            clusteringResult = res
            drawClusterCenters()
        }
        def getSrc = harrisFiltered


        // configurable vars

        var nClustersStep = 10
        var nClustersMaxTries = 100
        
        def tpe: TerminationCriteria.Type = TerminationCriteria.Type.Both // todo both for now
        var criteriaMaxCount: Int = 1000
        println("criteriaMaxCount = " + criteriaMaxCount)
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

        lazy val runner = Runner[Unit, List[((Int, Int), Double)], KMeansResult](
          _ => iPoints =>
            if(iPoints.length >= initialNClusters){
              val cData = iPoints.toMatOfPoint.convert(CvType.CV_32F).t()

              val best = new Mat()
              def centersPolicy = if(best.empty()) centersInitialPolicy
                                  else CentersPolicy.InitialLabels
              def kMeans(nClusters: Int) = {
                println("kMeans")
                kmeans(cData, nClusters, criteria, attempts, centersPolicy, best)
              }


              doUntil(initialNClusters, nClustersMaxTries)(
                nClust =>
                  kMeans(nClust) |> {
                    res =>
                      println(s"kmeans with $nClust clusters: compactness = ${res.compactness}")

                      if(res.compactness <= targetCompactness) scala.Right(res)
                      else                                  scala.Left(nClust+nClustersStep)
                  }).right
              .getOrElse(sys.error(s"Couldn't reach targetCompactness $targetCompactness in $nClustersMaxTries tries"))
            }
          else {
              Dialog.showMessage(parent = frame,
                                 message = "Number of clusters shouldn't be greater than number of points",
                                 title = "Warning",
                                 messageType = Dialog.Message.Warning)
              KMeansResult.empty
            }
        )

        // draw

        def drawClusterCenters()  = {
          affectImageMat(img => clusteringResult.centers.foreach(p => img.draw.circle(p.swap, 5, Color.blue)))
          repaintImage()
        }

      }












      lazy val configurationsOld: Config = new SimpleVerticalPanel with HarrisConfigurationPanelExec{
        def getSrc: Mat = ???
        def setResult: (Mat) => Unit = ???



        var threshold: Double = 0.1

        lazy val thresholdControl = controlForOrdered(threshold)(threshold = _)
                                    .spinner(new SpinnerNumberModel(threshold, 0, Double.PositiveInfinity, 0.001))

        var responseFunc: ResponseFunc = ResponseFunc.Original
        lazy val responseFuncControl = controlForSeq(ResponseFunc.all,  static = true)
                                       .dropDownList{
                                                      rf =>
                                                        responseFunc = rf
                                                        componentAccess.get("k").get.visible = rf == ResponseFunc.Original
                                                    }

        case class ResponseFunc(name: String, fromGray: Mat => Stream[((Int, Int), Double)]){
          override def toString = name
        }

        object ResponseFunc{
          def all = Original :: DetTrace :: DetTraceSq :: Nil

          object Original extends ResponseFunc("det - k*trace^2",
                                               grayImg =>
                                                 cornerHarris(grayImg, blockSize, kSize, k.toDouble, Option(borderType))
                                                   .mapV { case Array(d) => d }
                                                   .lazyPairs
                                                   .filter(_._2 > threshold)
                                               )
          object DetTrace   extends ResponseFunc("det / trace",   withEigenValues(cornerResponseDetTrace))
          object DetTraceSq extends ResponseFunc("det / trace^2", withEigenValues(cornerResponseDetTraceSq))

          def cornerResponseDetTrace:   EigenValsAndVecs => Double    =   eign => eign.det / eign.trace
          def cornerResponseDetTraceSq: EigenValsAndVecs => Double    =   eign => eign.det / math.pow(eign.trace, 2)

          def withEigenValues(response: EigenValsAndVecs => Double) =
            (grayImg: Mat) =>
              cornerEigenValsAndVecs(grayImg, blockSize, kSize, Option(borderType))
                .lazyPairs
                .mapVals(response)
                .filter(_._2 > threshold)
        }

        override def formBuilders: Seq[(String, (TestHarris.DSLFormBuilder[_], TestHarris.DSLLabelBuilder[_]))] =
          super.formBuilders/*.filterNot(_._1 == "k")*/ ++ Seq(
            "responseFunc" -> (responseFuncControl, label("Response Function")),
            "threshold" -> (thresholdControl -> label("Threshold"))
          )

        def kStep = Some(GuiArgModifier.Step(0.001))












        var initialNClusters = 250
        var nClustersStep = 10
        var nClustersMaxTries = 100

        var criteria = TerminationCriteria(TerminationCriteria.Type.Count, 1000, 1e-5)
        var attempts = 100
        var centersInitialPolicy: CentersPolicy = CentersPolicy.Random

        var maxCompactness = 1e4
        
        
        
        
        

        override lazy val runner: Runner[Params, Mat, Mat] = Runner {
          params =>
            src =>
              val cvt = ColorConversion(imageColorType, ColorMode.Gray)
              cvtColor(src, cvt) |> {
                grayImg =>

                  val filtered = responseFunc.fromGray(grayImg)
                  println("filtered.length = " + filtered.length)

                  val res = grayImg.convert(cvt.inverse)

                  filtered.foreach{ case ((i, j), r) => res.draw.circle(j -> i, 1, Color.red) }

                  lazy val initialNClusters = filtered.length / 20

                  if(filtered.length >= initialNClusters){
                    val cData = filtered.toMatOfPoint.convert(CvType.CV_32F).t()

                    val best = new Mat()
                    def centersPolicy = if(best.empty()) centersInitialPolicy
                                        else CentersPolicy.InitialLabels
                    def kMeans(nClusters: Int) = {
                      println("kMeans")
                      kmeans(cData, nClusters, criteria, attempts, centersPolicy, best)
                    }

                    val KMeansResult(ccenters, bestLabels, compactness) = {
                      println("search KMeansResult")
                      doUntil(initialNClusters, nClustersMaxTries)(
                        nClust =>
                          kMeans(nClust) |> {
                            res =>
                              println(s"kmeans with $nClust clusters: compactness = ${res.compactness}")

                              if(res.compactness <= maxCompactness) scala.Right(res)
                              else                                  scala.Left(nClust+nClustersStep)
                          }).right
                      .getOrElse(sys.error(s"coulndn't find clusters with max compactness $maxCompactness in $nClustersMaxTries tries"))
                    }

                    println("bestLabels = " + bestLabels)
                    println("compactness = " + compactness)

                    ccenters.map(_.swap).foreach(res.draw.circle(_, 5, Color.blue))
                  }

                  res
              }



        }

      }

      frame.updateForms()
    }

  def doUntil[T, R](initial: T, maxTry: Int = 100)(f: T => Either[T, R]): Either[T, R] =
    Y[(T, Int), Either[T, R]](
      rec => {
        case (prev, c) =>
          f(prev) match {
            case Left(t) => if (c == maxTry)  Left(t)
                            else              rec(t -> (c+1))
            case right   => right
          }
      }
    )(initial -> 0)
}
