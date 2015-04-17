package feh.tec.cvis

import java.awt.Dimension
import java.awt.image.BufferedImage
import feh.tec.cvis.common.describe.ArgModifier.{MaxCap, MinCap}
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.Harris
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
    {
      type Config = SimpleVerticalPanel with HarrisConfigurationPanelElements

      lazy val configurations = new SimpleVerticalPanel with HarrisConfigurationPanelElements{
        lazy val elems: Map[String, Seq[Component with UpdateInterface]] =
          formBuilders.mapValues{
            case (form, label) => label.formMeta.form :: form.formMeta.form :: Nil
          }

        def kBounds: Option[(MinCap[Double], MaxCap[Double])] = None

        def updateImage: (Array[Array[Byte]]) => Unit = ???
      }
    }
}
