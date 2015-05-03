package feh.tec.cvis.common.cv

import feh.util._
import org.opencv.core.{Core, Mat}
import org.opencv.imgproc.Imgproc
import feh.tec.cvis.common.cv.Helper._

object CornerDetection extends CornerDetection

trait CornerDetection{

  // C++:  void cornerHarris(Mat src, Mat& dst, int blockSize, int ksize, double k, int borderType = BORDER_DEFAULT)

  /**
   * <p>Harris edge detector.</p>
   *
   * <p>The function runs the Harris edge detector on the image. Similarly to
   * "cornerMinEigenVal" and "cornerEigenValsAndVecs", for each pixel <em>(x,
   * y)</em> it calculates a <em>2x2</em> gradient covariance matrix
   * <em>M^((x,y))</em> over a <em>blockSize x blockSize</em> neighborhood. Then,
   * it computes the following characteristic:</p>
   *
   * <p><em>dst(x,y) = det M^((x,y)) - k * (tr M^((x,y)))^2</em></p>
   *
   * <p>Corners in the image can be found as the local maxima of this response map.</p>
   *
   * @param src Input single-channel 8-bit or floating-point image.
   * @param blockSize Neighborhood size (see the details on "cornerEigenValsAndVecs").
   * @param ksize Aperture parameter for the "Sobel" operator.
   * @param k Harris detector free parameter. See the formula below.
   * @param borderType Pixel extrapolation method. See "borderInterpolate".
   * @return the Harris detector responses
   *
   * @see <a href="http://docs.opencv.org/modules/imgproc/doc/feature_detection.html#cornerharris">org.opencv.imgproc.Imgproc.cornerHarris</a>
   */
  def cornerHarris(src: Mat, blockSize: Int, ksize: Int, k: Double, borderType: Option[BorderExtrapolationMethod] = None): Mat =
    new Mat() $${
      dist =>
        Imgproc.cornerHarris(src, dist, blockSize, ksize, k, borderType map (_.value) getOrElse Core.BORDER_DEFAULT)
    }

  // C++:  void cornerEigenValsAndVecs(Mat src, Mat& dst, int blockSize, int ksize, int borderType = BORDER_DEFAULT)

  /**
   * <p>Calculates eigenvalues and eigenvectors of image blocks for corner detection.</p>
   *
   * <p>For every pixel <em>p</em>, the function <code>cornerEigenValsAndVecs</code>
   * considers a <code>blockSize</code> <em>x</em> <code>blockSize</code>
   * neighborhood <em>S(p)</em>. It calculates the covariation matrix of
   * derivatives over the neighborhood as:</p>
   *
   * <p><em>M = sum(by: S(p))(dI/dx)^2 sum(by: S(p))(dI/dx dI/dy)^2
   * sum(by: S(p))(dI/dx dI/dy)^2 sum(by: S(p))(dI/dy)^2 </em></p>
   *
   * <p>where the derivatives are computed using the "Sobel" operator.</p>
   *
   * <p>After that, it finds eigenvectors and eigenvalues of <em>M</em> and stores
   * them in the destination image as <em>(lambda_1, lambda_2, x_1, y_1, x_2,
   * y_2)</em> where</p>
   * <ul>
   *   <li> <em>lambda_1, lambda_2</em> are the non-sorted eigenvalues of
   * <em>M</em>
   *   <li> <em>x_1, y_1</em> are the eigenvectors corresponding to
   * <em>lambda_1</em>
   *   <li> <em>x_2, y_2</em> are the eigenvectors corresponding to
   * <em>lambda_2</em>
   * </ul>
   *
   * <p>The output of the function can be used for robust edge or corner detection.</p>
   *
   * @param src Input single-channel 8-bit or floating-point image.
   * @param blockSize Neighborhood size (see details below).
   * @param ksize Aperture parameter for the "Sobel" operator.
   * @param borderType Pixel extrapolation method. See "borderInterpolate".
   * @return Matrix[EigenValsAndVecs]. It has the same size as  <code>src</code>.
   *
   * @see <a href="http://docs.opencv.org/modules/imgproc/doc/feature_detection.html#cornereigenvalsandvecs">org.opencv.imgproc.Imgproc.cornerEigenValsAndVecs</a>
   * @see org.opencv.imgproc.Imgproc#cornerHarris
   * @see org.opencv.imgproc.Imgproc#cornerMinEigenVal
   * @see org.opencv.imgproc.Imgproc#preCornerDetect
   */
  def cornerEigenValsAndVecs(src: Mat, blockSize: Int, ksize: Int, borderType: Option[BorderExtrapolationMethod] = None): Array[Array[EigenValsAndVecs]] =
    withMat{
      dist =>
        Imgproc.cornerEigenValsAndVecs(src, dist, blockSize, ksize, borderType map (_.value) getOrElse Core.BORDER_DEFAULT)
        dist.map(
          (i, j) => {
            case Array(lambda1, lambda2, x1, y1, x2, y2) => EigenValsAndVecs(lambda1.toFloat, x1.toFloat -> y1.toFloat,
                                                                             lambda2.toFloat, x2.toFloat -> y2.toFloat)
          }
        )
      }

  case class EigenValsAndVecs(lambda1: Float, vec1: (Float, Float),
                              lambda2: Float, vec2: (Float, Float)){
    def det = lambda1 * lambda2
    def trace = lambda1 + lambda2
  }

}
