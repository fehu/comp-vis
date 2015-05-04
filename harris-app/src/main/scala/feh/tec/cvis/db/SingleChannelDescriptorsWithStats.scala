package feh.tec.cvis.db

import java.nio.ByteBuffer
import java.util.UUID

import feh.tec.cvis.DescriptorsSupport.{ADescriptor, IDescriptor}
import feh.tec.cvis.common.{ChannelDescriptor, ImageDescriptor}
import feh.tec.cvis.common.cv.Helper._
import org.opencv.core.Point
import slick.ast.ColumnOption.{PrimaryKey, AutoInc}
import slick.driver.H2Driver.api._
import slick.lifted.ProvenShape

import scala.concurrent.ExecutionContext.Implicits.global

object SingleChannelDescriptorsWithStats{

  class ImageDescriptors(tag: Tag) extends Table[(UUID, String, Array[Byte], Int)](tag, "ImageDescriptors"){

    def id    : Rep[UUID]        = column[UUID]        ("id", O.PrimaryKey, AutoInc)
    def name  : Rep[String]      = column[String]      ("name")
    def image : Rep[Array[Byte]] = column[Array[Byte]] ("image")

    def sideLength               = column[Int]         ("side")

    def * : ProvenShape[(UUID, String, Array[Byte], Int)] = (id, name, image, sideLength)
  }

  val imageDescriptors = TableQuery[ImageDescriptors]

  class PointDescriptors(tag: Tag)
    extends Table[(UUID, Int, Int, Array[Byte], Double, Double, Double, Double)](tag, "PointDescriptor"){

    def pk = primaryKey("pk", (descriptorId, pointX, pointY))

    def descriptorId = column[UUID] ("descriptor_id")
    def pointX       = column[Int]  ("point_x")
    def pointY       = column[Int]  ("point_y")

    def data  = column[Array[Byte]] ("data")

    def mean  = column[Double]      ("mean")
    def std   = column[Double]      ("std")
    def range = column[Double]      ("range")
    def iqr   = column[Double]      ("iqr")

    def descriptor = foreignKey("descriptor_fk", descriptorId, imageDescriptors)(_.id,
                                                                                 onUpdate=ForeignKeyAction.Restrict,
                                                                                 onDelete=ForeignKeyAction.Restrict
                                                                                )

    def * = (descriptorId, pointX, pointY, data, mean, std, range, iqr)
  }

  val pointDescriptors = TableQuery[PointDescriptors]


  def dbWrite(d: IDescriptor) = {
    val id = UUID.randomUUID()
    DBIO.seq(
      imageDescriptors +=(id, d.name, d.originalImage, d.sideLength)
    , pointDescriptors ++= d.interestPoints.map {
        case (p, descr) =>
          ( id
          , p.x.toInt
          , p.y.toInt
          , toBytes(descr.channel.data)
          , descr.channel.mean
          , descr.channel.std
          , descr.channel.range
          , descr.channel.iqr
          )
      }
    )
  }

  def dbRead(id: UUID) = {
    val getIDescr = imageDescriptors
                      .filter(_.id === id)
                      .map(d => (d.name, d.image, d.sideLength))
    val getPDescr = pointDescriptors
                      .filter(_.descriptorId === id)
                      .map(d => (d.pointX, d.pointY, d.data, d.mean, d.std, d.range, d.iqr))

    getIDescr.result flatMap {
      case (name, image, side) :: Nil =>
        getPDescr.to[List].result map {
          pdb =>
            val pts = pdb.map {
              case (x, y, data, mean, std, range, iqr) =>
                (x -> y: Point) -> ADescriptor(side, toDoubles(side, data), mean, std, range, iqr)
            }
            IDescriptor(name, side, image, pts.toMap)(Some(id))
        }
    }
  }

  private def toBytes(arr: Array[Double]) = {
    val narr = Array.ofDim[Byte](arr.length * 8)
    val buff = ByteBuffer.wrap(narr)
    arr.foreach(buff.putDouble)
    narr
  }

  private def toDoubles(length: Int, bytes: Array[Byte]) = {
    for(_ <- 1 to length) yield ByteBuffer.wrap(bytes).getDouble
  }.toArray




/* http://stackoverflow.com/questions/2905556/how-can-i-convert-a-byte-array-into-a-double-and-back

public static byte[] toByteArray(double value) {
    byte[] bytes = new byte[8];
    ByteBuffer.wrap(bytes).putDouble(value);
    return bytes;
}

public static double toDouble(byte[] bytes) {
    return ByteBuffer.wrap(bytes).getDouble();
}
 */





}