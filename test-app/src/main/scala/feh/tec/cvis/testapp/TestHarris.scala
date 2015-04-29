package feh.tec.cvis.testapp

import java.awt.Dimension
import java.awt.image._
import feh.tec.cvis.common.Helper._
import feh.tec.cvis.common._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.{GuiArgModifier, Harris}
import feh.util._
import org.opencv.core.{Core, Mat}
import scala.swing.Component
import scala.swing.Swing._

object TestHarris extends DefaultApp("harris-test", 300 -> 300, 600 -> 800) with Harris{

  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

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
    {
      frame =>

      type Config = SimpleVerticalPanel with HarrisConfigurationPanelExec

      lazy val configurations = new SimpleVerticalPanel with HarrisConfigurationPanelExec{
        lazy val elems: Map[String, Seq[Component with UpdateInterface]] =
          formBuilders.mapValues{
            case (form, label) => label.formMeta.form :: form.formMeta.form :: Nil
          }

        def kStep = Some(GuiArgModifier.Step(0.001))

//        override lazy val runner: Runner[Params, Mat, Mat] = Runner(
//          params =>
//            CallDescriptor.WithScopeAndParams(ConvertColor.Descriptor, frame,
//                                              (ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None)) chain
//            CallDescriptor.WithScopeAndParams(Harris.Descriptor, frame,
//                                              (blockSize, kSize, k.toDouble, Option(borderType)))
//        )

        def isCorner: EigenValsAndVecs => Boolean = {
          eign =>
            val r = eign.det / math.pow(eign.trace, 2)
            math.abs(r) >= 1
        }


        override lazy val runner: Runner[Params, Mat, Mat] = Runner(
          params =>
            src =>
              cvtColor(src, ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None) |> {
                grayImg =>
                  val res = cornerEigenValsAndVecs(grayImg, blockSize, kSize, Option(borderType))

                  println("res = " + res)
                  println("res.length = " + res.size)

                  val filtered = res.lazyPairs.withFilter(_._2 |> isCorner)

                  println("filtered.length = " + res.length)

                  grayImg
              }

        )

      }

      frame.updateForms()
    }
}
