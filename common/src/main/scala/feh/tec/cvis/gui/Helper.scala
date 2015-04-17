package feh.tec.cvis.gui

import java.awt.image.BufferedImage

object Helper {
  @deprecated("used??")
  case class DoCaseType[R](protected val caseInt:     () => R,
                           protected val caseShort:   () => R,
                           protected val caseByteArr: () => R,
                           protected val caseByte:    () => R){
//    def caseInt(f: => R)      = copy(_caseInt     = () => f)
//    def caseShort(f: => R)    = copy(_caseShort   = () => f)
//    def caseByteArr(f: => R)  = copy(_caseByteArr = () => f)
//    def caseByte(f: => R)     = copy(_caseByte    = () => f)

    def apply(tpe: ImageType): R = tpe match {
      case ImageType.Int    => caseInt()
      case ImageType.UShort => caseShort()
      case ImageType.Byte   => caseByte()
    }

    def apply(img: BufferedImage): R = apply(imageType(img))
  }

  type ImageType = ImageType.Value
  object ImageType extends Enumeration{
    val Int, UShort, Byte = Value
  }

  def imageType(img: BufferedImage): ImageType = {
    import BufferedImage._
    img.getType match {
      case TYPE_INT_RGB | TYPE_INT_ARGB | TYPE_INT_ARGB_PRE | TYPE_INT_BGR                            => ImageType.Int
      case TYPE_USHORT_565_RGB | TYPE_USHORT_555_RGB | TYPE_USHORT_GRAY                               => ImageType.UShort
      case TYPE_BYTE_GRAY | TYPE_BYTE_BINARY | TYPE_3BYTE_BGR | TYPE_4BYTE_ABGR | TYPE_4BYTE_ABGR_PRE => ImageType.Byte
      case TYPE_BYTE_INDEXED => sys.error("no idea")
    }
  }
}
