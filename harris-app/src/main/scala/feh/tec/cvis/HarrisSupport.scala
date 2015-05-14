package feh.tec.cvis

import java.awt.Color

import feh.dsl.swing2.Var
import feh.tec.cvis.common.cv.Drawing._
import feh.tec.cvis.common.cv.Helper._
import feh.tec.cvis.common.cv._
import feh.tec.cvis.common.cv.describe.ArgModifier.MinCap
import feh.tec.cvis.common.cv.describe.CallHistory.ArgEntry
import feh.tec.cvis.common.cv.describe.{ArgDescriptor, CallDescriptor, CallHistory, CallHistoryContainer}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.GuiArgModifier.Step
import feh.tec.cvis.gui.configurations.{ConfigBuildHelper, Harris}
import feh.util._
import org.opencv.core.{Point, Mat}

trait HarrisSupport extends Harris with InterestPointSearchSupport{
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  trait HarrisSupportFrame
    extends ConfigurationsPanelBuilder
    with HistorySupport
    with InterestPointSearchSupportFrame
    with HarrisGUI
    with ColorConverting
  {
    frame:     GenericSimpleAppFrame
          with FrameExec
          with LayoutDSL
          with GuiFrame
          with ConfigBuildHelperGUI =>

    type HarrisResult  = List[((Int, Int), Double)]
    type HarrisFilterd = List[Point]

    protected var harrisResult  : Var[CallHistoryContainer[HarrisResult]]  = Var(CallHistoryContainer.empty(Nil))
    protected var harrisFiltered: Var[CallHistoryContainer[HarrisFilterd]] = Var(CallHistoryContainer.empty(Nil))

    protected var filteredInterestPointsCount: Int = 0
    protected var initialNClusters: Int = 1


    object HarrisPanel
      extends SimpleVerticalPanel
      with HarrisConfigurationPanelExec
      with MatPanelExec
      with PanelExecSimple[Mat, Mat]
      with InterestPointSearchPanel
    {
      panel =>

      def steps = 2

      def kStep = Some(Step(0.001))

      def getSrc = originalMat
      def setResult: Mat => Unit = _ => drawHarris()


      def callDescriptor = CallDescriptor(describe.Harris.Descriptor.name)
      def params: Set[ArgEntry[_]] = Set(
        ArgEntry(describe.Harris.BlockSize, blockSize)
      , ArgEntry(describe.Harris.KSize, kSize)
      , ArgEntry(describe.Harris.K, k)
      , ArgEntry(ResponseFuncDescriptor, responseFunc)
      )

      def setHResult: ((Mat, CallHistory[Mat])) => Unit = _ => drawHarris()

      object ResponseFuncDescriptor extends ArgDescriptor[ResponseFunc]("Response function", null)
      object Threshold extends ArgDescriptor[Double]("Threshold", "ignore values below", MinCap(0), Step(0.001))

      var responseFunc: ResponseFunc  = ResponseFunc.Original
      var threshold   : Double        = 0.1

      lazy val responseFuncControl = controlForSeq(ResponseFunc.all,  static = true)
                                     .dropDownList{
                                                    rf =>
                                                      responseFunc = rf
                                                      componentAccess.get("k").get.visible = rf == ResponseFunc.Original
                                                  }

      lazy val thresholdControl    = mkNumericControl(Threshold)(threshold, threshold = _) |> fixPreferredSize

      lazy val applyThresholdButton = triggerFor{
        setHarrisFiltered( filterHarris(harrisResult.get.value) )
        if(repaint_?.get) {
          drawHarris()
          tabs.tryUpdate()
        }
      }.button("Apply Threshold")


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

      def calcInitialNClusters(c: Int) =  if      (c > 1000)  c / 100
                                          else if (c > 10)    c / 10
                                          else                1

      def setHarrisFiltered(res: HarrisFilterd) = {
        harrisFiltered set harrisResult.get.affect(harrisHistory)(_ => res)
        filteredInterestPointsCount = res.length
        initialNClusters = calcInitialNClusters(filteredInterestPointsCount)
      }

      override lazy val runner: Runner[Params, Mat, Mat] = Runner {
        nextStep =>
          params =>
            src =>
              withGrayImg(src){
                grayImg =>
                  nextStep()
                  val responses = responseFunc.fromGray(grayImg).toList
                  harrisResult set CallHistoryContainer(responses, CallHistory.Empty)            // TODO !!! no side effects should be present here
                  nextStep()
                  setHarrisFiltered( filterHarris(responses) )
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

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      lazy val FilterHarris = CallDescriptor("filter harris points of interest")
      
      def harrisHistory       = CallHistory.Entry(callDescriptor, params)
      def filterHarrisHistory = CallHistory.Entry(FilterHarris, Set(ArgEntry(Threshold, threshold)))

      def filterHarris: HarrisResult => HarrisFilterd = _.withFilter(_._2 >= threshold).map(_._1: Point)

      lazy val showFilteredInterestPointsCount  = monitorFor(s"Filtered interest points: $filteredInterestPointsCount").text

      override def formBuilders: Seq[(String, (DSLFormBuilder[_], DSLLabelBuilder[_]))] =
        super.formBuilders ++ Seq(
          "responseFunc"                  -> (responseFuncControl -> label(ResponseFuncDescriptor.name)),
          "threshold"                     -> thresholdControl,
          "applyThreshold"                -> (applyThresholdButton            -> label("")),
          "filteredInterestPointsCount"   -> (showFilteredInterestPointsCount -> label(""))
        )

      def drawHarris() = {
        if(repaint_?.get) Option(originalInGrayScale) map (_.clone()) foreach setImageMat
        affectImageMat(img => harrisFiltered.get.value.foreach{ p => img.draw.circle(p.swap, 1, Color.red) })
        repaintImage()
      }
    }
  }
}
