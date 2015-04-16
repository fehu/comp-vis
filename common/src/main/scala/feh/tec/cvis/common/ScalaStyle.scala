package feh.tec.cvis.common

import java.awt.image.{DataBufferByte, BufferedImage}
import java.io.File

import org.opencv.core.{MatOfInt, MatOfKeyPoint, Mat}
import org.opencv.features2d.FeatureDetector
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import feh.util._
import scala.collection.convert.decorateAll._


/*
 *                Image IO
 */

trait ImageIO{
  type WriteParams = Map[Int, Int] // instead of MatOfInt

  def imRead(file: String, flags: ImRead.LoadColorType = ImRead.Color): Mat = Highgui.imread(file, flags.value)
//  def imread(file: Path): Mat = imread(file.mkString(File.separator))

  def imWrite(filename: String, img: Mat, params: WriteParams = Map()) = {
    val p = params.map{ case (k, v) => k :: v :: Nil }.flatten.toSeq
    if(p.nonEmpty) Highgui.imwrite(filename, img,  new MatOfInt(p: _*))
  }
}

/*
 *                Creating Mat
 */

trait MatCreation{
  implicit class BufferedImageToMatWrapper(img: BufferedImage){
    def toMat: Mat = {
      val m = new Mat(img.getHeight, img.getWidth, img.getType) // ?? type ??
      m put(0, 0, img.getData.getDataBuffer.asInstanceOf[DataBufferByte].getData)
      m
    }
  }
}

// from nu.pattern/opencv/srcs/opencv-2.4.9-7-sources.jar!/org/opencv/highgui/Highgui.java
object ImRead{
  abstract class LoadColorType(val value: Int)
  
  case object Unchanged extends LoadColorType(-1)
  case object Grayscale extends LoadColorType(0)
  case object Color     extends LoadColorType(1)
  case object AnyDepth  extends LoadColorType(2)
  case object AnyColor  extends LoadColorType(4)
}

/*
 *                Border Extrapolation
 */

sealed abstract class BorderExtrapolationMethod(val value: Int)

// from nu.pattern/opencv/srcs/opencv-2.4.9-7-sources.jar!/org/opencv/imgproc/Imgproc.java
object BorderExtrapolationMethod {
  case object Constant    extends BorderExtrapolationMethod(0)
  case object Replicate   extends BorderExtrapolationMethod(1)
  case object Reflect     extends BorderExtrapolationMethod(2)
  case object Wrap        extends BorderExtrapolationMethod(3)
  case object Reflect_101 extends BorderExtrapolationMethod(4)
  case object Transparent extends BorderExtrapolationMethod(5)
  case object Isolated    extends BorderExtrapolationMethod(16)
  
  def Default = Reflect_101
}


/*
 *                Corner Detection
 */

object CornerDetection extends CornerDetection

trait CornerDetection{
  def cornerHarris(src: Mat, blockSize: Int, ksize: Int, k: Double, borderType: Option[Int] = None): Mat = {
    val dist = src.clone()
    borderType map (Imgproc.cornerHarris(src, dist, blockSize, ksize, k, _)) getOrElse
                    Imgproc.cornerHarris(src, dist, blockSize, ksize, k)
    dist
  }

}


/*
 *                Feature Detection
 */

object FeatureDetection extends FeatureDetection

trait FeatureDetection{
  type MatWithMaskMap = Map[Mat, Mat]
  
  def create(tpe: FeatureDetectionType) = FeatureDetector.create(tpe.value)

  val feature = FeatureDetectionType

  implicit class DetectWrapper(detector: FeatureDetector){

    def detect(image: Mat, mask: Mat = null): MatOfKeyPoint = new MatOfKeyPoint() $$ {
      _keys => Option(mask)
        .map(       detector.detect(image, _keys, _) )
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