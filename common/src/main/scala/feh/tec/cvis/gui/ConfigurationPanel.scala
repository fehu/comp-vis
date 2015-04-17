package feh.tec.cvis.gui

import feh.dsl.swing.{AbstractGUI, SwingAppBuildingEnvironment}

import scala.swing.Panel

trait ConfigurationPanel extends Panel{
//  def updateImage: Array[Array[Byte]] => Unit

}

/** uses swing-dsl */
trait GenericConfigurationGUI {
  gui: AbstractGUI =>

  protected trait GenericGUIFrame {
    frame: GuiFrame =>

    trait GenericConfigurationPanel extends ConfigurationPanel with FormCreationDSL{
      //    monitorFor()
      //    controlFor()
      //    ...
    }

  }
}
