package feh.tec.cvis.gui.configurations

import feh.tec.cvis.common.cv.describe.ArgModifier

trait GuiArgModifier[T] extends ArgModifier[T]

object GuiArgModifier{
  case class Step[T](get: T) extends GuiArgModifier[T]
}
