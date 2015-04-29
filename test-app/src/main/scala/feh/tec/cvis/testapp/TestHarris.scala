package feh.tec.cvis.testapp

import java.awt.Dimension
import java.awt.image._
import feh.tec.cvis.common.describe.ArgModifier.{MaxCap, MinCap}
import feh.tec.cvis.common.describe.Harris
import feh.tec.cvis.common._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.{GuiArgModifier, Harris}
import org.opencv.core.{CvType, Core, Mat}
import scala.reflect.ClassTag
import scala.swing.Component
import scala.swing.Swing._
import feh.util._

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

        override lazy val runner: Runner[Params, Mat, Mat] = Runner(
          params =>
            src =>
              cvtColor(src, ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None) |> {
                grayImg =>
//                  println("grayImg = " + grayImg)
//                  grayImg.convertTo(grayImg, CvType.CV_8U) // todo mutating!
//                  println("grayImg = " + grayImg)
                  val res = cornerHarris(grayImg, blockSize, kSize, k.toDouble, Option(borderType))
                  println("res = " + res)
                  println("res is zero " + res.get(0,0).forall(_ == 0))
                  res
              }

        )

      }

      frame.updateForms()
    }
}
