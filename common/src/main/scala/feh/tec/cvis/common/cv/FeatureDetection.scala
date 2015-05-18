package feh.tec.cvis.common.cv

import feh.util._
import org.opencv.core.{Mat, MatOfKeyPoint}
import org.opencv.features2d.FeatureDetector

import scala.collection.convert.decorateAll._

object FeatureDetection extends FeatureDetection

trait FeatureDetection{
  type MatWithMaskMap = Map[Mat, Mat]

  def create(tpe: FeatureDetectionTypeRoot) = FeatureDetector.create(tpe.value)

  def feature = FeatureDetectionType

  implicit class DetectWrapper(detector: FeatureDetector){

    // C++:  void javaFeatureDetector::detect(Mat image, vector_KeyPoint& keypoints, Mat mask = Mat())
    /** Detects keypoints in an image (first variant) or image set (second variant).
     *
     * @param image Image.
     * @param mask Mask specifying where to look for keypoints (optional). It must
     * be a 8-bit integer matrix with non-zero values in the region of interest.
     * @return The detected keypoints. In the second variant of the method
     * <code>keypoints[i]</code> is a set of keypoints detected in <code>images[i]</code>.
     * @see <a href="http://docs.opencv.org/modules/features2d/doc/common_interfaces_of_feature_detectors.html#featuredetector-detect">org.opencv.features2d.FeatureDetector.detect</a>
     */
    def detect(image: Mat, mask: Mat = null): MatOfKeyPoint = new MatOfKeyPoint() $$ {
      _keys => Option(mask)
               .map(       detector.detect(image, _keys, _) )               // todo !!!
               .getOrElse( detector.detect(image, _keys) )
    }

    def detect(images: List[Mat]): List[MatOfKeyPoint] = {
      val _keys = images.map(_ => new MatOfKeyPoint()).asJava

      detector.detect(images.asJava, _keys)
      _keys.asScala.toList
    }

    def detect(images: MatWithMaskMap): List[MatOfKeyPoint] = {
      val _keys = images.map(_ => new MatOfKeyPoint()).toList.asJava
      val imgs  = images.keys.toList.asJava
      val masks = images.values.toList.asJava

      detector.detect(imgs, _keys, masks)
      _keys.asScala.toList
    }
  }
}

sealed trait FeatureDetectionTypeRoot{
  val value: Int

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: FeatureDetectionTypeRoot => this.value == that.value
  }
}
sealed abstract class FeatureDetectionType(val value: Int) extends FeatureDetectionTypeRoot
sealed abstract class FeatureDetectionTypeModifier(val modifier: Int)

// from nu.pattern/opencv/srcs/opencv-2.4.9-7-sources.jar!/org/opencv/features2d/FeatureDetector.java
object FeatureDetectionType {
  /** FastFeatureDetector */
  case object Fast        extends FeatureDetectionType(1)
  /** StarFeatureDetector */
  case object Star        extends FeatureDetectionType(2)
  /** "SIFT" (nonfree module) */
  case object SIFT        extends FeatureDetectionType(3)
  /** "SURF" (nonfree module) */
  case object SURF        extends FeatureDetectionType(4)
  case object ORB         extends FeatureDetectionType(5)
  case object MSER        extends FeatureDetectionType(6)
  /** GoodFeaturesToTrackDetector */
  case object GFTT        extends FeatureDetectionType(7)
  /** "GoodFeaturesToTrackDetector" with Harris detector enabled */
  case object Harris      extends FeatureDetectionType(8)
  /** SimpleBlobDetector */
  case object SimpleBlob  extends FeatureDetectionType(9)
  /** DenseFeatureDetector */
  case object Dense       extends FeatureDetectionType(10)
  case object BRISK       extends FeatureDetectionType(11)

  object modifier{
    case object None            extends FeatureDetectionTypeModifier(0)
    case object GridDetector    extends FeatureDetectionTypeModifier(1000)
    case object PyramidDetector extends FeatureDetectionTypeModifier(2000)
    case object DynamicDetector extends FeatureDetectionTypeModifier(3000)

    def list = None :: GridDetector :: PyramidDetector :: DynamicDetector :: Nil
  }

  implicit class FeatureDetectionTypeModiftWrapper(tpe: FeatureDetectionType) {
    def ::(mod: FeatureDetectionTypeModifier) = FeatureDetectionTypeModified(tpe, mod)
  }

  def list = Fast :: Star :: SIFT :: SURF :: ORB :: MSER :: GFTT :: Harris :: SimpleBlob :: Dense :: BRISK :: Nil
}

case class FeatureDetectionTypeModified(tpe: FeatureDetectionType, modifier: FeatureDetectionTypeModifier)
  extends FeatureDetectionTypeRoot
{
  lazy val value = tpe.value + modifier.modifier
}
