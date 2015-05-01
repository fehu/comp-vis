package feh.tec.cvis.gui.configurations

import feh.dsl.swing.AbstractGUI
import feh.tec.cvis.common.describe.{ArgDescriptor, ArgModifier, Harris}
import feh.tec.cvis.common.{BorderExtrapolationMethod, CornerDetection}
import feh.tec.cvis.gui.{GenericConfigurationGUI, GenericSimpleAppFrameImplementation}
import feh.util._
import org.opencv.core.Mat
import scala.reflect.ClassTag
import scala.swing.Alignment

trait Harris extends GenericConfigurationGUI with CornerDetection{
  gui: AbstractGUI with GenericSimpleAppFrameImplementation =>

  trait HarrisGUI extends super.GenericGUIFrame{
    frame: GuiFrame with FrameExec =>

    trait HarrisConfigurationPanelExec extends MatPanelExec{ //
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



      protected def getParams(): Params = (blockSize, kSize, k, Option(borderType)) 

      lazy val runner: Runner[Params, Mat, Mat] = Runner.create(Harris.Descriptor.call(gui))



      def kStep: Option[GuiArgModifier.Step[BigDecimal]]
      protected def kArgs = kStep.map(_ &: Harris.K) getOrElse Harris.K
      protected def kSizeArgs = GuiArgModifier.Step(2) &: Harris.KSize

      lazy val blockSizeBuilder  = mkNumericControl(Harris.BlockSize)(blockSize, _blockSize = _)
      lazy val kSizeBuilder      = mkNumericControl(kSizeArgs)(kSize, _kSize = _)
      lazy val kBuilder          = mkNumericControl(kArgs)(k, _k = _)
      lazy val borderTypeBuilder = mkListControl(Harris.BorderType, borderTypes)(_borderType = _, _.asString)

      def borderTypes = (BorderExtrapolationMethod.Default :: BorderExtrapolationMethod.all).distinct

      def mkNumericControl[N: Ordering](descr: ArgDescriptor[N])(get: => N, set: N => Unit)
                      (implicit num: Numeric[N]): (DSLFormBuilder[N], DSLLabelBuilder[_]) =
      {
        val caps = descr.modifiers.collect{
          case max: ArgModifier.MaxCap[_] => max
          case min: ArgModifier.MinCap[_] => min
        }

        import num._

        lazy val max = caps.collectFirst{ case ArgModifier.MaxCap(mx) => mx }
        lazy val min = caps.collectFirst{ case ArgModifier.MinCap(mn) => mn }
        lazy val step = descr.modifiers.collectFirst{ case GuiArgModifier.Step(s) => s }
                    .getOrElse{
                                num match {
                                  case num: Integral[N]   => num.one
                                  case num: Fractional[N] => num.div(max.get - min.get, num.fromInt(100))
                                }
                              }
        lazy val domain = Stream.iterate(min.get)(_ + step).takeWhile(_ <= max.get).toList

        lazy val pos  = caps exists { case _: ArgModifier.Positive[_] => true   ; case _ => false }
        lazy val nneg = caps.exists{ case _: ArgModifier.NonNegative[_] => true ; case _ => false }


        val control =
          if(caps.size == 2) controlGivenDomain(get)(set).slider(domain, _.Left)
          else controlForNumeric(get)(set).spinner() |> {
            cntr =>
              max map cntr.maxValue getOrElse cntr |> {
                cntr =>
                  min map{
                    minvalue => cntr.minValue(if(pos) num.min(minvalue, num.zero) else minvalue)
                  } getOrElse
                    (if(pos) cntr.minValue(num.zero) else cntr)
                  cntr.step(step)
              }
          }

        (control, mkControlLabel(descr))
      }

      def mkListControl[T: ClassTag](descr: ArgDescriptor[T], domain: List[T])(set: T => Unit, asString: T => String) =
        (controlForSeq(domain, static = true).dropDownList(set)
                                             .withStringExtractor(asString)
        ,  mkControlLabel(descr))
      }

    protected def mkControlLabel(descr: ArgDescriptor[_]) =
      monitorFor(
        <html>
          <dl>
            <dt>{descr.name}</dt>
            <dd>
              <span style='font-size:8px'>{descr.description}</span>
            </dd>
          </dl>
        </html>
          .toString()
      )
        .label
        .affect( _.horizontalAlignment = Alignment.Left)

  }

}
