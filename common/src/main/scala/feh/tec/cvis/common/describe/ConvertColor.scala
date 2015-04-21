package feh.tec.cvis.common.describe

import feh.tec.cvis.common.ColorMode
import org.opencv.core.Mat
import org.opencv.imgproc.Imgproc
import feh.util._

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
object ConvertColor {

  object ConvertCode extends ArgDescriptor[ColorMode]("Code", "color space conversion code")

  object ChannelsNumber extends ArgDescriptor[Option[Int]]("Resulting number of channels",
                                                           "number of channels in the destination image; " +
                                                             "if the parameter is 0, the number of the channels is derived " +
                                                             "automatically from src and code .")

  object Descriptor extends CallDescriptor[Any, Mat, (Int, Option[Int]), Mat](
    _ => mat => {
      case (cvtCode, dstCnOpt) => new Mat() $$ ( Imgproc.cvtColor(mat, _, cvtCode, dstCnOpt getOrElse 0) )
    }
  )
}
