package feh.tec.cvis

import feh.util._
import feh.tec.cvis.common.cv.{ColorMode, ColorConversion, ColorConverting}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import org.opencv.core.Mat

trait InterestPointSearchSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  trait InterestPointSearchSupportFrame
    extends ConfigurationsPanelBuilder
    with HistorySupport
    with ColorConverting
  {
    frame:     GenericSimpleAppFrame
          with FrameExec
          with LayoutDSL
          with ConfigBuildHelperGUI =>

    protected var originalGray: Mat = null
    protected var originalInGrayScale: Mat = null

    trait InterestPointSearchPanel
      extends SimpleVerticalPanel {
      panel: PanelExec[_, _] =>


      protected def withGrayImg[R](original: Mat)(fGray: Mat => R): R = {
        val grayImg = Option(originalGray).getOrElse {
          val cvt = ColorConversion(imageColorType, ColorMode.Gray)
          cvtColor(original, cvt) |> {
            grayI =>
              originalGray = grayI
              originalInGrayScale = grayI.convert(cvt.inverse)
              grayI
          }
        }
        fGray(grayImg)
      }
    }
  }
}
