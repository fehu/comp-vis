package feh.tec.cvis.common

import java.awt.image.{DataBufferByte, BufferedImage}
import java.io.File

import org.opencv.core.{CvType, MatOfInt, MatOfKeyPoint, Mat}
import org.opencv.features2d.FeatureDetector

import org.opencv.imgproc.Imgproc
import feh.util._
import scala.collection.convert.decorateAll._
import scala.reflect.ClassTag


/*
 *                Image IO
 */
//import org.opencv.highgui.Highgui

//trait ImageIO{
//  type WriteParams = Map[Int, Int] // instead of MatOfInt
//
//  def imRead(file: String, flags: ImRead.LoadColorType = ImRead.Color): Mat = Highgui.imread(file, flags.value)
////  def imread(file: Path): Mat = imread(file.mkString(File.separator))
//
//  def imWrite(filename: String, img: Mat, params: WriteParams = Map()) = {
//    val p = params.map{ case (k, v) => k :: v :: Nil }.flatten.toSeq
//    if(p.nonEmpty) Highgui.imwrite(filename, img,  new MatOfInt(p: _*))
//  }
//}

/*
 *                Creating Mat
 */

@deprecated
trait MatCreation{
//  implicit class BufferedImageToMatWrapper(img: BufferedImage){
//    def toMat: Mat = {
//      val m = new Mat(img.getHeight, img.getWidth, img.getType)
//      m put(0, 0, img.getData.getDataBuffer.asInstanceOf[DataBufferByte].getData) // todo ??? DataBufferByte
//      m
//    }
//  }

//  def mkMat[N: CanFillMat](width: Int, height: Int, tpe: Int, data: Array[N]): Mat =
//    new Mat(height, width, tpe) $$ ( implicitly[CanFillMat[N]].mk(_, data) )
//
//  trait CanFillMat[From]{
//    def mk(mat: Mat, arr: Array[From])
//  }
//
//  trait CanExtractMat[As]{
//    def get(mat: Mat): Array[As]
//  }

  //{ _.put(0, 0, data) }
  //.get(0, 0, data)
}

// from nu.pattern/opencv/srcs/opencv-2.4.9-7-sources.jar!/org/opencv/highgui/Highgui.java
//object ImRead{
//  abstract class LoadColorType(val value: Int)
//
//  case object Unchanged extends LoadColorType(-1)
//  case object Grayscale extends LoadColorType(0)
//  case object Color     extends LoadColorType(1)
//  case object AnyDepth  extends LoadColorType(2)
//  case object AnyColor  extends LoadColorType(4)
//}

/*
 *                Border Extrapolation
 */

sealed abstract class BorderExtrapolationMethod(val value: Int, val asString: String)

// from nu.pattern/opencv/srcs/opencv-2.4.9-7-sources.jar!/org/opencv/imgproc/Imgproc.java
object BorderExtrapolationMethod {
  case object Constant    extends BorderExtrapolationMethod(0,  "Constant")
  case object Replicate   extends BorderExtrapolationMethod(1,  "Replicate")
  case object Reflect     extends BorderExtrapolationMethod(2,  "Reflect")
  case object Wrap        extends BorderExtrapolationMethod(3,  "Wrap")
  case object Reflect_101 extends BorderExtrapolationMethod(4,  "Reflect 101")
  case object Transparent extends BorderExtrapolationMethod(5,  "Transparent")
  case object Isolated    extends BorderExtrapolationMethod(16, "Isolated")
  
  def Default = Reflect_101

  def all = List(Constant, Replicate, Reflect, Wrap, Reflect_101, Transparent, Isolated)
}

/*
 *                Colors
 */

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
  def cvtColor(src: Mat, conv: ColorConversion, dstCnOpt: Option[Int]): Mat = {
    val convCode = ColorConversion.code(conv) getOrThrow s"no conversion code for $conv"
    new Mat() $$ (Imgproc.cvtColor(src, _, convCode, dstCnOpt getOrElse 0))
  }
}

case class ColorConversion(from: ColorMode, to: ColorMode)

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

  def mode(img: BufferedImage): ColorMode = img.getType match {
    case BufferedImage.TYPE_3BYTE_BGR | BufferedImage.TYPE_INT_BGR  => ColorMode.BGR
    case BufferedImage.TYPE_4BYTE_ABGR                              => ColorMode.BGRA
    case BufferedImage.TYPE_INT_RGB                                 => ColorMode.RGB
    case BufferedImage.TYPE_INT_ARGB                                => ColorMode.RGBA
    case BufferedImage.TYPE_BYTE_GRAY                               => ColorMode.Gray
  }

}

//object MatColorMode {
//  import ColorMode._
//
//  def code[T: ClassTag](colorMode: ColorMode): Int = ???
//
//  lazy val homograficMapping: Map[ColorMode, Int] = Map(
//    RGB   -> CvType.CV2
//
//  )
//  lazy val reprDependantMapping: Map[(ColorMode, ClassTag[_]), Int] = ???
//}


/*
 *                Corner Detection
 */

object CornerDetection extends CornerDetection

trait CornerDetection{
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
  def cornerHarris(src: Mat, blockSize: Int, ksize: Int, k: Double, borderType: Option[BorderExtrapolationMethod] = None): Mat = {
    val dist = src.clone()
    borderType map (_.value) map (Imgproc.cornerHarris(src, dist, blockSize, ksize, k, _)) getOrElse
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
