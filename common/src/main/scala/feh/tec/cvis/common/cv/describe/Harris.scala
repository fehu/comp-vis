package feh.tec.cvis.common.cv.describe

import feh.tec.cvis.common.cv.describe.ArgModifier._
import feh.tec.cvis.common.cv.{BorderExtrapolationMethod, CornerDetection}
import org.opencv.core.Mat

/** Harris edge detector arguments descriptions
 *
 *  @see [[CornerDetection.cornerHarris]] (in common/src/main/scala/feh/tec/cvis/common/ScalaStyle.scala)
 */

object Harris {
  type Params = (Int, Int, BigDecimal, Option[BorderExtrapolationMethod])

  /** Neighborhood size. */
  object BlockSize  extends ArgDescriptor[Int]("Block size", "neighborhood size", Positive)

  /** Aperture parameter for the "Sobel" operator. */
  object KSize      extends ArgDescriptor[Int]("k size", "aperture parameter for the \"Sobel\" operator", Positive)

  /** Harris detector free parameter.*/
  object K          extends ArgDescriptor[BigDecimal]("k", "Harris detector free parameter", MinCap(0.01), MaxCap(0.1))

  /** Pixel extrapolation method. */
  object BorderType extends ArgDescriptor[BorderExtrapolationMethod]("Border type", "pixel extrapolation method", Optional)

  object Descriptor extends CallDescriptor[CornerDetection, Mat, Params, Mat](
    scope => mat => {
      case (blockSize, kSize, k, borderType) => scope.cornerHarris(mat, blockSize, kSize, k.toDouble, borderType)
    }
  )
}
