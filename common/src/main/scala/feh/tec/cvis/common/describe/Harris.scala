package feh.tec.cvis.common.describe

import feh.tec.cvis.common.{CornerDetection, BorderExtrapolationMethod}
import feh.tec.cvis.common.describe.ArgModifier._
import org.opencv.core.Mat

/** Harris edge detector arguments descriptions
 *
 *  @see [[feh.tec.cvis.common.CornerDetection.cornerHarris]] (in common/src/main/scala/feh/tec/cvis/common/ScalaStyle.scala)
 */

object Harris {

  /** Neighborhood size. */
  object BlockSize  extends ArgDescriptor[Int]("Block size", "neighborhood size", Integer, Positive)

  /** Aperture parameter for the "Sobel" operator. */
  object KSize      extends ArgDescriptor[Int]("k size", "aperture parameter for the \"Sobel\" operator", Integer, Positive)

  /** Harris detector free parameter.*/
  object K          extends ArgDescriptor[Double]("k", "Harris detector free parameter") //(MaxCap(1), MinCap(-1))

  /** Pixel extrapolation method. */
  object BorderType extends ArgDescriptor[BorderExtrapolationMethod]("Border type", "pixel extrapolation method", Optional)

  object Descriptor extends CallDescriptor[CornerDetection, Mat, (Int, Int, Double, Option[BorderExtrapolationMethod]), Mat](
    scope => mat => {
      case (blockSize, kSize, k, borderType) => scope.cornerHarris(mat, blockSize, kSize, k, borderType)
    }
  )
}
