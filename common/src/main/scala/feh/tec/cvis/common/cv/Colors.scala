package feh.tec.cvis.common.cv

import java.awt.image.BufferedImage

import feh.util._
import org.opencv.core.Mat
import org.opencv.imgproc.Imgproc


abstract class ColorMode

/** OpenCV color modes */
object ColorMode{
  case object RGB   extends ColorMode
  case object BGR   extends ColorMode
  case object BGRA  extends ColorMode
  case object RGBA  extends ColorMode
  case object Gray  extends ColorMode
  // and much more

  def values = Set(RGB, RGBA, BGR, BGRA, Gray)
}


/** Converts an image from one color space to another.
  * C++: void cvtColor(InputArray src, OutputArray dst, int code, int dstCn=0 )
  *
  * Parameters:
  *   src – input image: 8-bit unsigned, 16-bit unsigned ( CV_16UC... ), or single-precision floating-point.
  *   dst – output image of the same size and depth as src.
  *   code – color space conversion code (see the description below).
  *   dstCn – number of channels in the destination image; if the parameter is 0, the number of the channels is derived automatically from src and code .
  *
  * @see http://docs.opencv.org/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor
  */
trait ColorConverting {
  def cvtColor(src: Mat, conv: ColorConversion, dstCnOpt: Option[Int] = None): Mat = {
    val convCode = ColorConversion.code(conv) getOrThrow s"no conversion code for $conv"
    new Mat() $$ (Imgproc.cvtColor(src, _, convCode, dstCnOpt getOrElse 0))
  }

  implicit class ColorConvertWrapper(img: Mat){
    def convert(conv: ColorConversion, dstCnOpt: Option[Int] = None) = cvtColor(img, conv, dstCnOpt)
  }
}


case class ColorConversion(from: ColorMode, to: ColorMode){
  def inverse = ColorConversion(to, from)
}

object ColorConversion{
  import ColorMode._

  def code(from: ColorMode, to: ColorMode): Option[Int] = conversionCodes.get(from).flatMap(_.get(to))
  def code(c: ColorConversion): Option[Int] = code(c.from, c.to)

  lazy val conversionCodes: Map[ColorMode, Map[ColorMode, Int]] = Map(
    RGB   -> Map( BGR  -> Imgproc.COLOR_RGB2BGR
                , BGRA -> Imgproc.COLOR_RGB2BGRA
                , RGBA -> Imgproc.COLOR_RGB2RGBA
                , Gray -> Imgproc.COLOR_RGB2GRAY
    ),

    BGR   -> Map( RGB  -> Imgproc.COLOR_BGR2RGB
                , RGBA -> Imgproc.COLOR_BGR2RGBA
                , BGRA -> Imgproc.COLOR_BGR2BGRA
                , Gray -> Imgproc.COLOR_BGR2GRAY
    ),

    BGRA  -> Map( RGB  -> Imgproc.COLOR_BGRA2RGB
                , RGBA -> Imgproc.COLOR_BGRA2RGBA
                , BGR  -> Imgproc.COLOR_BGRA2BGR
                , Gray -> Imgproc.COLOR_BGRA2GRAY
    ),

    RGBA  -> Map( BGR  -> Imgproc.COLOR_RGBA2BGR
                , BGRA -> Imgproc.COLOR_RGBA2BGRA
                , RGB  -> Imgproc.COLOR_RGBA2RGB
                , Gray -> Imgproc.COLOR_RGBA2GRAY
    ),

    Gray  -> Map( BGR  -> Imgproc.COLOR_GRAY2BGR
                , BGRA -> Imgproc.COLOR_GRAY2BGRA
                , RGB  -> Imgproc.COLOR_GRAY2RGB
                , RGBA -> Imgproc.COLOR_GRAY2RGBA
    )
  )
}

object BufferedImageColor{

  def mode(img: BufferedImage): ColorMode = mode(img.getType)

  def mode(tpe: Int): ColorMode = tpe match {
    case BufferedImage.TYPE_3BYTE_BGR | BufferedImage.TYPE_INT_BGR  => ColorMode.BGR
    case BufferedImage.TYPE_4BYTE_ABGR                              => ColorMode.BGRA
    case BufferedImage.TYPE_INT_RGB                                 => ColorMode.RGB
    case BufferedImage.TYPE_INT_ARGB                                => ColorMode.RGBA
    case BufferedImage.TYPE_BYTE_GRAY                               => ColorMode.Gray
  }

}