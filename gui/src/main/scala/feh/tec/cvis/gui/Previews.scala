package feh.tec.cvis.gui

import java.awt.image.BufferedImage

import feh.dsl.swing2.Var

import scala.swing.Swing._
import scala.swing._
import scala.swing.event.{Key, MouseMoved}

trait SimplePreview extends ScrollPane{

  def img: BufferedImage

  horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded

  private def w = img.getWidth
  private def h = img.getHeight

  val underlying = new Component {
    maximumSize = w -> h
    minimumSize = w -> h
    preferredSize = w -> h

    override def paint(g: Graphics2D): Unit = g.drawImage(img, 0, 0, null)
  }

  this.contents = underlying
}

trait PreviewMouseReaction{
  self: SimplePreview =>

  def onMouseMovement: (Point, Key.Modifiers) => Unit

  listenTo(underlying.mouse.moves)
  reactions += {
    case MouseMoved(`underlying`, p, k) => onMouseMovement(p, k)
  }
}

trait PreviewHighlights{
  self: SimplePreview =>

  val highlights: Var[Set[Point]] = Var(Set())

  def highlightColor: Color

  override def paint(g: Graphics2D): Unit = {
    g.drawImage(img, 0, 0, null)
    g.setColor(highlightColor)
    highlights.get.foreach(p => g.drawOval(p.y - 1, p.x - 1, 2, 2))
  }


}
