package feh.tec.cvis.common.cv

import feh.util._
import org.opencv.core.{Mat, MatOfKeyPoint}
import org.opencv.features2d.FeatureDetector

import scala.collection.convert.decorateAll._

object FeatureDetection extends FeatureDetection

trait FeatureDetection{
  type MatWithMaskMap = Map[Mat, Mat]

  def create(tpe: FeatureDetectionType) = FeatureDetector.create(tpe.value)

  val feature = FeatureDetectionType

  implicit class DetectWrapper(detector: FeatureDetector){

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

sealed trait FeatureDetectionTypeRoot{ val value: Int }
sealed abstract class FeatureDetectionType(val value: Int) extends FeatureDetectionTypeRoot
sealed abstract class FeatureDetectionTypeModifier(val modifier: Int)

// from nu.pattern/opencv/srcs/opencv-2.4.9-7-sources.jar!/org/opencv/features2d/FeatureDetector.java
object FeatureDetectionType {
  case object Fast        extends FeatureDetectionType(1)
  case object Star        extends FeatureDetectionType(2)
  case object SIFT        extends FeatureDetectionType(3)
  case object SURF        extends FeatureDetectionType(4)
  case object ORB         extends FeatureDetectionType(5)
  case object MSER        extends FeatureDetectionType(6)
  case object GFTT        extends FeatureDetectionType(7)
  case object Harris      extends FeatureDetectionType(8)
  case object SimpleBlob  extends FeatureDetectionType(9)
  case object Dense       extends FeatureDetectionType(10)
  case object BRISK       extends FeatureDetectionType(11)

  case object GridDetector    extends FeatureDetectionTypeModifier(1000)
  case object PyramidDetector extends FeatureDetectionTypeModifier(2000)
  case object DynamicDetector extends FeatureDetectionTypeModifier(3000)

  implicit class FeatureDetectionTypeModiftWrapper(tpe: FeatureDetectionType) {
    def ::(mod: FeatureDetectionTypeModifier) = FeatureDetectionTypeModified(tpe, mod)
  }
}

case class FeatureDetectionTypeModified(tpe: FeatureDetectionType, modifier: FeatureDetectionTypeModifier)
  extends FeatureDetectionTypeRoot
{
  lazy val value: Int = tpe.value + modifier.modifier
}
