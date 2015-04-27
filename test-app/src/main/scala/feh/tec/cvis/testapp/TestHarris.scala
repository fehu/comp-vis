package feh.tec.cvis.testapp

import java.awt.Dimension
import java.awt.image._
import feh.tec.cvis.common.describe.ArgModifier.{MaxCap, MinCap}
import feh.tec.cvis.common.describe.Harris
import feh.tec.cvis.common.{CornerDetection, MatCreation}
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.Harris
import org.opencv.core.{CvType, Core, Mat}
import scala.reflect.ClassTag
import scala.swing.Component
import scala.swing.Swing._

object TestHarris extends DefaultApp("test", 300 -> 300, 600 -> 800) with Harris{

  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit ) =
    new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
      with ConfigurationsPanelBuilder
      with HarrisGUI
      with FrameExec
      with CornerDetection
    {
      frame =>

      type Config = SimpleVerticalPanel with HarrisConfigurationPanelExec

      lazy val configurations = new SimpleVerticalPanel with HarrisConfigurationPanelExec{
        lazy val elems: Map[String, Seq[Component with UpdateInterface]] =
          formBuilders.mapValues{
            case (form, label) => label.formMeta.form :: form.formMeta.form :: Nil
          }

        def kBounds = Some(MinCap(-10.0) -> MaxCap(10.0))

      }




      // tags
//      @deprecated("not needed?")
//      def underlyingTag(buffi: BufferedImage): ClassTag[_] = buffi.getRaster.getDataBuffer match {
//        case _: DataBufferInt     => scala.reflect.classTag[Int]
//        case _: DataBufferFloat   => scala.reflect.classTag[Float]
//        case _: DataBufferDouble  => scala.reflect.classTag[Double]
//        case _: DataBufferByte    => scala.reflect.classTag[Byte]
//      }

//      def underlyingTag(mat: Mat): ClassTag[_] = mat.

      // Mat support
//      def toMat(buffi: BufferedImage): Mat = underlyingTag(buffi) match {
//        case ClassTag.Int => ???
//          val cv = buffi.getType match{
//            case BufferedImage.TYPE_INT_ARGB
//          }
//          new Mat(buffi.getHeight, buffi.getWidth, CvType.CV)
//      }


      //    def underlyingTag(mat: Mat): ClassTag[_]
      def toMat(img: BufferedImage): Mat = {
        val buff = img.getRaster.getDataBuffer
        val depth = DataBuffer.getDataTypeSize(buff.getDataType) / 8
        val chanels = buff.getSize / (img.getHeight * img.getWidth)

        val mat = new Mat(img.getHeight, img.getWidth, CvType.makeType(depth, chanels))
        buff match {
          case b: DataBufferByte    => mat.put(0, 0, b.getData)
          case b: DataBufferShort   => mat.put(0, 0, b.getData)
          case b: DataBufferUShort  => mat.put(0, 0, b.getData)
          case b: DataBufferInt     => mat.put(0, 0, b.getData)
          case b: DataBufferFloat   => mat.put(0, 0, b.getData)
          case b: DataBufferDouble  => mat.put(0, 0, b.getData: _*)
        }
        mat
      }

      def toBufferImage(mat: Mat): BufferedImage = {
        val gray  = mat.channels() == 1
        val norm  = mat.channels() == 3
        val alpha = mat.channels() == 4

        val setByte  = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferByte].getData)
        val setShort = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferShort].getData)
        val setInt   = (buff: DataBuffer) => mat.get(0, 0, buff.asInstanceOf[DataBufferInt].getData)

        // todo ???
        val (tpe, setData) = mat.depth() * 8 match {
          // Byte
          case 8  if gray  => BufferedImage.TYPE_BYTE_GRAY  -> setByte
          case 8  if norm  => BufferedImage.TYPE_3BYTE_BGR  -> setByte
          case 8  if alpha => BufferedImage.TYPE_4BYTE_ABGR -> setByte
          // Short | UShort
          case 16 if gray  => BufferedImage.TYPE_USHORT_GRAY -> setShort
          case 16          => ???
          // Int | Float
          case 32 if gray  => ???
          case 32 if norm  => BufferedImage.TYPE_INT_RGB  -> setInt
          case 32 if alpha => BufferedImage.TYPE_INT_ARGB -> setInt
          // Double
          case 64          => ???
        }

        val img = new BufferedImage(mat.width(), mat.height(), tpe)
        setData(img.getRaster.getDataBuffer)
        img
      }
    }
}
