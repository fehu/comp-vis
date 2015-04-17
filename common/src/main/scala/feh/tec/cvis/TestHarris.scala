package feh.tec.cvis

import java.awt.Dimension
import java.awt.image.BufferedImage

import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.Harris
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
        lazy val elems: Map[String, DSLFormBuilder[Any]#FormBuildMeta] = formBuilders.mapValues(_.formMeta)
        def updateImage: (Array[Array[Byte]]) => Unit = ???
      }
    }
}
