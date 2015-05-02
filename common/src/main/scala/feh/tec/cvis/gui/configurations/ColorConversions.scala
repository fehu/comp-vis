package feh.tec.cvis.gui.configurations

import feh.dsl.swing.AbstractGUI
import feh.tec.cvis.common.{BufferedImageColor, ColorMode, ColorConversion, ColorConverting}
import feh.tec.cvis.common.describe.ConvertColor
import feh.tec.cvis.gui.{GenericSimpleAppFrameImplementation, GenericConfigurationGUI}
import org.opencv.core.Mat

trait ColorConversions extends GenericConfigurationGUI with ColorConverting{
  gui: AbstractGUI with GenericSimpleAppFrameImplementation =>

  trait ColorConversionGUI extends super.GenericGUIFrame {
    frame: GuiFrame with FrameExec =>

    trait ColorConversionPanelExec extends MatPanelExec{
      conf: GenericConfigurationPanel =>

      final type Params = ConvertColor.Params

//      lazy val convertFromControl = controlForSeq(ColorMode.values.toSeq, static = true).dropDownList(convertFrom = _)
      lazy val convertToControl   = controlForSeq(ColorMode.values.toSeq, static = true).dropDownList(convertTo = _)

      protected def convertFrom: ColorMode = BufferedImageColor.mode(modifiedImage)
      protected var convertTo: ColorMode = ColorMode.values.head

      def convertCode: ColorConversion = ColorConversion(convertFrom, convertTo)
      def channelsNumber: Option[Int]  = None

      def getParams(): Params = (convertCode, channelsNumber)

      def runner: Runner[Params, Mat, Mat] = Runner.atomic(ConvertColor.Descriptor.call(gui))

      def updateForms(): Unit = ???
    }
  }
}
