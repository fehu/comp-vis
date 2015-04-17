package feh.tec.cvis.common.describe

import feh.tec.cvis.common.BorderExtrapolationMethod
import feh.tec.cvis.common.describe.ArgModifier._

/** Harris edge detector arguments descriptions
 *
 *  @see [[feh.tec.cvis.common.CornerDetection.cornerHarris]]
 */

object Harris {

  /** Neighborhood size. */
  object BlockSize  extends ArgDescriptor[Int]("Block size", "neighborhood size", ArgModifier.Integer, Positive)

  /** Aperture parameter for the "Sobel" operator. */
  object KSize      extends ArgDescriptor[Int]("k size", "aperture parameter for the \"Sobel\" operator", ArgModifier.Integer, Positive)

  /** Harris detector free parameter.*/
  object K          extends ArgDescriptor[Double]("k", "Harris detector free parameter") //(MaxCap(1), MinCap(-1))

  /** Pixel extrapolation method. */
  object BorderType extends ArgDescriptor[BorderExtrapolationMethod]("Border type", "pixel extrapolation method")
}
