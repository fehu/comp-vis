package feh.tec.cvis.gui.configurations

import feh.dsl.swing.AbstractGUI
import feh.tec.cvis.common.describe.{ArgModifier, ArgDescriptor}
import feh.tec.cvis.gui.{GenericSimpleAppFrameImplementation, GenericConfigurationGUI}
import feh.util._
import scala.reflect.ClassTag
import scala.swing.Alignment

trait ConfigBuildHelper extends GenericConfigurationGUI {
  gui: AbstractGUI with GenericSimpleAppFrameImplementation =>

  trait ConfigBuildHelperGUI extends super.GenericGUIFrame {
    frame: GuiFrame with FrameExec =>

    trait ConfigBuildHelperPanel {
      executable: GenericConfigurationPanel with PanelExec[_, _] =>

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
