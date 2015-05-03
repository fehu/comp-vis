package feh.tec.cvis

import java.awt.Color
import javax.swing.SpinnerNumberModel

import feh.tec.cvis.common.cv.Drawing._
import feh.tec.cvis.common.cv.Helper._
import feh.tec.cvis.common.cv._
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.{ConfigBuildHelper, GuiArgModifier, Harris}
import feh.util._
import org.opencv.core.Mat

trait HarrisSupport extends Harris{
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  trait HarrisSupportFrame
    extends ConfigurationsPanelBuilder
    with HarrisGUI
    with ColorConverting
  {
    frame:     GenericSimpleAppFrame
          with FrameExec
          with LayoutDSL
          with GuiFrame
          with ConfigBuildHelperGUI =>

    protected var originalGray: Mat = null

    protected var harrisResult  : List[((Int, Int), Double)] = Nil
    protected var harrisFiltered: List[((Int, Int), Double)] = Nil

    protected var filteredInterestPointsCount: Int = 0
    protected var initialNClusters: Int = 1


    object HarrisPanel extends SimpleVerticalPanel with HarrisConfigurationPanelExec{
      panel =>

      def steps = 2

      def kStep = Some(GuiArgModifier.Step(0.001))

      def getSrc = originalMat
      def setResult: Mat => Unit = _ => drawHarris()

      var threshold: Double = 0.1
      lazy val thresholdControl = controlForOrdered(threshold)(threshold = _)
                                  .spinner(new SpinnerNumberModel(threshold, 0, Double.PositiveInfinity, 0.001))

      lazy val applyThresholdButton = triggerFor{
        if(repaint_?.get) setHarrisFiltered( filterHarris(harrisResult) )
        drawHarris()
        tabs.tryUpdate()
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

      def calcInitialNClusters(c: Int) =  if      (c > 1000)  c / 100
                                          else if (c > 10)    c / 10
                                          else                1

      def setHarrisFiltered(seq: Seq[((Int, Int), Double)]) = {
        harrisFiltered = seq.toList
        filteredInterestPointsCount = harrisFiltered.length
        initialNClusters = calcInitialNClusters(filteredInterestPointsCount)
      }

      override lazy val runner: Runner[Params, Mat, Mat] = Runner {
        nextStep =>
          params =>
            src =>
              val cvt = ColorConversion(imageColorType, ColorMode.Gray)
              cvtColor(src, cvt) |> {
                grayImg =>
                  originalGray = grayImg.convert(cvt.inverse)
                  nextStep()
                  val responses = responseFunc.fromGray(grayImg)
                  harrisResult = responses.toList            // TODO !!! no side effects should be present here
                  nextStep()
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

      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      def filterHarris: Seq[((Int, Int), Double)] => Seq[((Int, Int), Double)] = _.filter(_._2 >= threshold)

      lazy val showFilteredInterestPointsCount  = monitorFor(s"Filtered interest points: $filteredInterestPointsCount").text

      override def formBuilders: Seq[(String, (DSLFormBuilder[_], DSLLabelBuilder[_]))] =
        super.formBuilders ++ Seq(
          "responseFunc"                  -> (responseFuncControl -> label("Response Function")),
          "threshold"                     -> (thresholdControl    -> label("Threshold")),
          "applyThreshold"                -> (applyThresholdButton            -> label("")),
          "filteredInterestPointsCount"   -> (showFilteredInterestPointsCount -> label(""))
        )

      def drawHarris() = {
        if(repaint_?.get) Option(originalGray) map (_.clone()) foreach setImageMat
        affectImageMat(img => harrisFiltered.foreach{ case ((i, j), r) => img.draw.circle(j -> i, 1, Color.red) })
        repaintImage()
      }
    }
  }
}
