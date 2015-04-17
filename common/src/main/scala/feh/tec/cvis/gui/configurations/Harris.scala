package feh.tec.cvis.gui.configurations

import java.awt.Color

import feh.dsl.swing.AbstractGUI
import feh.tec.cvis.common.describe.ArgModifier.{MinCap, MaxCap}
import feh.tec.cvis.common.{describe, BorderExtrapolationMethod, CornerDetection}
import feh.tec.cvis.common.describe.{Harris, ArgModifier, ArgDescriptor}
import feh.tec.cvis.gui.GenericConfigurationGUI
import feh.util._

import scala.collection.immutable.NumericRange
import scala.reflect.ClassTag
import scala.swing.{Swing, Alignment}

trait Harris extends GenericConfigurationGUI with CornerDetection{
  gui: AbstractGUI =>

  trait HarrisGUI extends super.GenericGUIFrame{
    frame: GuiFrame =>

    trait HarrisConfigurationPanelElements { //
      self: GenericConfigurationPanel =>

      def formBuilders: Map[String, (DSLFormBuilder[_], DSLLabelBuilder[_])] = Map(
        "block-size"  -> blockSizeBuilder,
        "k-size"      -> kSizeBuilder,
        "k"           -> kBuilder,
        "border-type" -> borderTypeBuilder
      )


      protected var blockSize: Int = 1
      protected var kSize: Int = 1
      protected var k: Double = 0
      protected var borderType: BorderExtrapolationMethod = borderTypes.head

      def kBounds: Option[(MinCap[Double], MaxCap[Double])] // MinCap(-10.0), MaxCap(10.0)
      protected def kArgs = kBounds.map(p => p._1 &: p._2 &: Harris.K) getOrElse Harris.K

      lazy val blockSizeBuilder  = mkNumericControl(Harris.BlockSize)(blockSize, blockSize = _)
      lazy val kSizeBuilder      = mkNumericControl(Harris.KSize)(kSize, kSize = _)
      lazy val kBuilder          = mkNumericControl(kArgs)(k, k = _)
      lazy val borderTypeBuilder = mkListControl(Harris.BorderType, borderTypes)(borderType = _, _.asString)

      def borderTypes = (BorderExtrapolationMethod.Default :: BorderExtrapolationMethod.all).distinct

      def mkNumericControl[N: Ordering](descr: ArgDescriptor[N])(get: => N, set: N => Unit)
                      (implicit num: Numeric[N]): (DSLFormBuilder[N], DSLLabelBuilder[_]) =
      {
        val caps = descr.modifiers.collect{
          case max: ArgModifier.MaxCap[_] => max
          case min: ArgModifier.MinCap[_] => min
        }

        val max = caps.collectFirst{ case ArgModifier.MaxCap(mx) => mx }
        val min = caps.collectFirst{ case ArgModifier.MinCap(mn) => mn }
        val pos  = caps exists { case _: ArgModifier.Positive[_] => true   ; case _ => false }
        val nneg = caps.exists{ case _: ArgModifier.NonNegative[_] => true ; case _ => false }

//        println(s"control for $descr")
//        println(s"\tcaps = $caps, max = $max. min = $min, positive = $pos, not-negative = $nneg")

        val control =
          if(caps.size == 2)  controlForNumeric(get)(set).slider(min.get, max.get, num.one)
          else controlForOrdered(get)(set).spinner |> {
            cntr =>
              max map cntr.maxValue getOrElse cntr |> {
                cntr => min map{
                  minvalue => cntr.minValue(if(pos) num.min(minvalue, num.zero) else minvalue)
                } getOrElse
                  (if(pos) cntr.minValue(num.zero) else cntr)
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

//    protected def mkControlLabel(descr: ArgDescriptor[_]) =
//      monitorFor(
//        s"""<html>
//           |  <span style='font-size:8px'>
//           |    <b>${descr.name}</b> - ${descr.description}
//           |  </span>
//           |</html>""".stripMargin
//      ).label
//        .affect( _.horizontalAlignment = Alignment.Left,
//                 .affect( _.horizontalAlignment = Alignment.Left,
//        )


  }


//  abstract class VerticalConfigurationPanel(val updateImage: Array[Array[Byte]] => Unit)
//    extends GenericConfigurationPanel with ConfigurationPanelElements
//  {
//    lazy val configPanel = panel
//  }

}
