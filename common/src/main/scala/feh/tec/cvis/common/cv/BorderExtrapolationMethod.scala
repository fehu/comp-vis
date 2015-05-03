package feh.tec.cvis.common.cv

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