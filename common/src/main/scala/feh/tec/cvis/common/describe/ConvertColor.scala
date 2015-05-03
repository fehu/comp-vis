package feh.tec.cvis.common.describe

import feh.tec.cvis.common.ColorConversion
import feh.tec.cvis.common.cv.{ColorConversion, ColorConverting}
import org.opencv.core.Mat
import org.opencv.imgproc.Imgproc
import feh.util._

/** Converts an image from one color space to another.
 *
 * @see [[ColorConverting]] (in common/src/main/scala/feh/tec/cvis/common/ScalaStyle.scala)
 */
object ConvertColor {
  type Params = (ColorConversion, Option[Int])

  object ConvertCode extends ArgDescriptor[ColorConversion]("Code", "color space conversion code")

  object ChannelsNumber extends ArgDescriptor[Option[Int]]("Resulting number of channels",
                                                           "number of channels in the destination image; " +
                                                             "if the parameter is 0, the number of the channels is derived " +
                                                             "automatically from src and code .")

  object Descriptor extends CallDescriptor[ColorConverting, Mat, (ColorConversion, Option[Int]), Mat](
    cc => mat => {
      case (cvtCode, dstCnOpt) => cc.cvtColor(mat, cvtCode, dstCnOpt)
    }
  )
}
