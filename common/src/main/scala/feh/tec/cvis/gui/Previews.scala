package feh.tec.cvis.gui

import java.awt.image.BufferedImage

import scala.swing.Swing._
import scala.swing._

trait SimplePreview extends ScrollPane{

  def img: BufferedImage  //val img = ImageIO.read(imageSrc)

  horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
  verticalScrollBarPolicy   = ScrollPane.BarPolicy.AsNeeded

  private def w = img.getWidth
  private def h = img.getHeight

  val underlying = new Component {
    maximumSize = w -> h
    minimumSize = w -> h
    preferredSize = w -> h

    //      new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    override def paint(g: Graphics2D): Unit = g.drawImage(img, 0, 0, null)
  }

  this.contents = underlying
}
