/*
package feh.tec.cvis.testapp

import java.awt.Dimension
import java.awt.image.BufferedImage
import feh.tec.cvis.common._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import org.opencv.core.Mat
import scala.swing.Component
import scala.swing.Swing._
import feh.util._

object Tst extends DefaultApp("test", 300 -> 300, 600 -> 800)
  with CornerDetection
  with ColorConverting
{
  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: (Tst.SimpleFrame) => Unit,
                    unregAFrame: (Tst.SimpleFrame) => Unit): SimpleFrame =
    new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
      with ConfigurationsPanelBuilder
      with FrameExec
      with MatSupport
    {
      type Config = SimpleVerticalPanel with MatPanelExec

      val configurations: Config = new SimpleVerticalPanel with MatPanelExec{
        type Params = ()

        val elems: Map[String, Seq[Component with Tst.UpdateInterface]] = Map()

        protected def getParams(): Params = ()

        def runner: Runner[Params, Mat, Mat] = Runner(
          params =>
            src =>
              cvtColor(src, ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None) |> {
                grayImg => cornerHarris(grayImg, )
              }

        )

      }
    }
}
*/
