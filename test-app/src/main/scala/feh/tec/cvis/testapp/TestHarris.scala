package feh.tec.cvis.testapp

import java.awt.Dimension
import java.awt.image.BufferedImage

import feh.tec.cvis.common.describe.ArgModifier.{MaxCap, MinCap}
import feh.tec.cvis.common.describe.Harris
import feh.tec.cvis.common.{CornerDetection, MatCreation}
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.Harris
import org.opencv.core.Mat

import scala.reflect.ClassTag
import scala.swing.Component
import scala.swing.Swing._

object TestHarris extends DefaultApp("test", 300 -> 300, 600 -> 800) with Harris{

  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit ) =
    new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
      with ConfigurationsPanelBuilder
      with HarrisGUI
      with FrameExecMatSupport
      with FrameExecRunners
      with CornerDetection
      with MatCreation
    {
      frame =>

      type Config = SimpleVerticalPanel with HarrisConfigurationPanelElements

      lazy val configurations = new SimpleVerticalPanel with HarrisConfigurationPanelElements{
        lazy val elems: Map[String, Seq[Component with UpdateInterface]] =
          formBuilders.mapValues{
            case (form, label) => label.formMeta.form :: form.formMeta.form :: Nil
          }

        def kBounds = Some(MinCap(-10.0) -> MaxCap(10.0))

      }

      def mkRunner = {
        val args = (configurations.blockSize, configurations.kSize, configurations.k, Option(configurations.borderType))
        runnerFor( in => Harris.Descriptor.call(frame)(in)(args) ) _
      }


/*
      abstract class HarrisRunner[N: CanFillMat: CanExtractMat: ClassTag] extends Runner[N] with MatSupport[N]{

        def execTyped(): ImageHolder[N] = {
          Harris.Descriptor.call(frame)(snapshotToMat(snapshot))(    // .asInstanceOf[ModifiedBufferSnapshot[N]]
            (configurations.blockSize, configurations.kSize, configurations.k, Option(configurations.borderType))
          )
        }

        //          val data = implicitly[CanExtractMat[N]].get(mat)
        //            Array.ofDim[N](mat.width * mat.height * mat.depth)

        //          ImageHolder(mat.width, mat.height, mat.`type`(), data)



      }
*/

    }
}
