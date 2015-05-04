package feh.tec.cvis.gui.configurations

import javax.swing.SpinnerNumberModel

import feh.dsl.swing.AbstractGUI
import feh.dsl.swing.util.AwtUtils
import feh.tec.cvis.common.cv.describe.{ArgDescriptor, ArgModifier}
import feh.tec.cvis.gui.{GenericConfigurationGUI, GenericSimpleAppFrameImplementation}
import feh.util._

import scala.math.Numeric.{BigDecimalIsFractional, DoubleIsFractional, FloatIsFractional, IntIsIntegral}
import scala.reflect.ClassTag
import scala.swing.{Alignment, Component}

trait ConfigBuildHelper extends GenericConfigurationGUI {
  gui: AbstractGUI with GenericSimpleAppFrameImplementation =>

  trait ConfigBuildHelperGUI extends super.GenericGUIFrame {
    frame: GuiFrame with FrameExec =>

    trait ConfigBuildHelperPanel extends AwtUtils{
      executable: GenericConfigurationPanel with PanelExec[_, _] =>

      def formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))]


      protected def mkElems: Seq[(String, Seq[Component])] =
        formBuilders.mapVals{ case (c, label) => label.component :: c.component :: Nil }

      lazy val elems = mkElems



      def mkNumericControl[N: Ordering](descr: ArgDescriptor[N])(get: => N, set: N => Unit)
                      (implicit num: Numeric[N]): (DSLFormBuilder[N], DSLLabelBuilder[_]) =
      {
        val caps = descr.modifiers.collect{
          case max: ArgModifier.MaxCap[_] => max
          case min: ArgModifier.MinCap[_] => min
        }

        import num._

        import Ordered._

        lazy val maxOpt = caps.collectFirst{ case ArgModifier.MaxCap(mx) => mx }
        lazy val max: N = maxOpt.getOrElse( num match {
                                         case _: DoubleIsFractional     => Double.MaxValue.asInstanceOf[N]
                                         case _: BigDecimalIsFractional => BigDecimal(10000).asInstanceOf[N]
                                         case _: FloatIsFractional      => Float.MaxValue.asInstanceOf[N]
                                         case _: IntIsIntegral          => Int.MaxValue.asInstanceOf[N]
                                       })

        lazy val minOpt = caps.collectFirst{ case ArgModifier.MinCap(mn) => mn }
        lazy val min: N = minOpt.orElse[N]{ descr.modifiers.collectFirst{
                                      case _: ArgModifier.NonNegative[_] => num.zero
                                      case _: ArgModifier.Positive[_] => num match {
                                              case _: DoubleIsFractional      => Double.MinPositiveValue.asInstanceOf[N]
                                              case _: BigDecimalIsFractional  => BigDecimal(Double.MinPositiveValue).asInstanceOf[N]
                                              case _: FloatIsFractional       => Float.MinPositiveValue.asInstanceOf[N]
                                              case _: IntIsIntegral           => num.one
                                            }
                                    }
                                  }
                           .getOrElse[N](num match {
                                        case _: DoubleIsFractional      => Double.MinValue.asInstanceOf[N]
                                        case _: BigDecimalIsFractional  => BigDecimal(Double.MinValue).asInstanceOf[N]
                                        case _: FloatIsFractional       => Float.MinValue.asInstanceOf[N]
                                        case _: IntIsIntegral           => Int.MinValue.asInstanceOf[N]
                                      }
                           )
        lazy val step = descr.modifiers
                        .collectFirst{ case GuiArgModifier.Step(s) => s }
                        .orElse(PartialFunction.condOpt(maxOpt -> minOpt) {
                                  case (Some(mn), Some(mx)) if num.isInstanceOf[Fractional[_]] =>
                                    num.asInstanceOf[Fractional[N]].div(mx - mn, num.fromInt(100))
                                })
                        .getOrElse(num.one)

        lazy val domain = Stream.iterate(min)(_ + step).takeWhile(_ <= max).toList

        lazy val pos  = caps exists { case _: ArgModifier.Positive[_] => true   ; case _ => false }
        lazy val nneg = caps.exists{ case _: ArgModifier.NonNegative[_] => true ; case _ => false }


        def toNumber(n: N) = n match {
          case b: BigDecimal  => b.bigDecimal
          case b: BigInt      => b.bigInteger
          case x: Double      => Double.box(x)
          case i: Int         => Int.box(i)
        }

        def toNumberN(n: N) = toNumber(n).asInstanceOf[N]

//        def fromNumber(a: Any) = a match {
//          case b: java.math.BigDecimal  => BigDecimal(b).asInstanceOf[N]
//          case b: BigInteger            => BigInt(b).asInstanceOf[N]
//          case _                        => a.asInstanceOf[N]
//        }

        val control =
          if(caps.size == 2) controlGivenDomain(get)(set).slider(domain, _.Left)
          else {
            val m = new SpinnerNumberModel(toNumber(min), toNumber(min): Comparable[_], toNumber(max), toNumber(step))
            controlForNumeric(get)(set).spinner(m)
          }
        (control, mkControlLabel(descr))
      }

      def mkListControl[T: ClassTag](descr: ArgDescriptor[T], domain: List[T])(set: T => Unit, asString: T => String) =
        (controlForSeq(domain, static = true).dropDownList(set)
                                             .withStringExtractor(asString)
        ,  mkControlLabel(descr))


      def fixPreferredSize[B <: AbstractDSLBuilder]: ((B, DSLLabelBuilder[_])) => (B, DSLLabelBuilder[_]) = {
        case (c, l) => c.affect(x => x.preferredSize = 0 -> x.preferredSize._2).asInstanceOf[B] -> l
      }

    }


    protected def mkControlLabel(descr: ArgDescriptor[_]) =
      monitorFor(
        <html>{
          Option(descr.description) map { dd =>
            <dl>
              <dt>{descr.name}</dt>
              <dd> <span style='font-size:8px'>{dd}</span> </dd>
            </dl>
          } getOrElse
            <dl>
              <dt>{descr.name}</dt>
            </dl>
          }</html>
        .toString()
      )
      .label
      .affect( _.horizontalAlignment = Alignment.Left)

/*
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
     */
  }
}
