package feh.tec.cvis.gui.configurations

import feh.dsl.swing.AbstractGUI
import feh.tec.cvis.common.describe.Harris
import feh.tec.cvis.common.{BorderExtrapolationMethod, CornerDetection}
import feh.tec.cvis.gui.{GenericConfigurationGUI, GenericSimpleAppFrameImplementation}
import org.opencv.core.Mat

trait Harris extends GenericConfigurationGUI with CornerDetection with ConfigBuildHelper{
  gui: AbstractGUI with GenericSimpleAppFrameImplementation =>

  trait HarrisGUI extends super.GenericGUIFrame with ConfigBuildHelperGUI{
    frame: GuiFrame with FrameExec =>

    trait HarrisConfigurationPanelExec extends MatPanelExec with ConfigBuildHelperPanel{ //
      conf: GenericConfigurationPanel =>

      final type Params = Harris.Params
      
      def formBuilders: Seq[(String, (DSLFormBuilder[_], DSLLabelBuilder[_]))] = Seq(
        "block-size"  -> blockSizeBuilder,
        "k-size"      -> kSizeBuilder,
        "k"           -> kBuilder,
        "border-type" -> borderTypeBuilder
      )


      protected var _blockSize: Int = 1
      protected var _kSize: Int = 1
      protected var _k: BigDecimal = 0.05
      protected var _borderType: BorderExtrapolationMethod = borderTypes.head

      final def blockSize = _blockSize
      final def kSize = _kSize
      final def k = _k
      final def borderType = _borderType



      def getParams(): Params = (blockSize, kSize, k, Option(borderType))

      lazy val runner: Runner[Params, Mat, Mat] = Runner.atomic(Harris.Descriptor.call(gui))



      def kStep: Option[GuiArgModifier.Step[BigDecimal]]
      protected def kArgs = kStep.map(_ &: Harris.K) getOrElse Harris.K
      protected def kSizeArgs = GuiArgModifier.Step(2) &: Harris.KSize

      lazy val blockSizeBuilder  = mkNumericControl(Harris.BlockSize)(blockSize, _blockSize = _)
      lazy val kSizeBuilder      = mkNumericControl(kSizeArgs)(kSize, _kSize = _)
      lazy val kBuilder          = mkNumericControl(kArgs)(k, _k = _)
      lazy val borderTypeBuilder = mkListControl(Harris.BorderType, borderTypes)(_borderType = _, _.asString)

      def borderTypes = (BorderExtrapolationMethod.Default :: BorderExtrapolationMethod.all).distinct

    }

  }

}
