package feh.tec.cvis.common

import feh.tec.cvis.common.ChannelDescriptor.Comparator
import feh.tec.cvis.common.cv.Helper.Array2D
import org.opencv.core.Point



trait ImageDescriptor {
  type ADescriptor <: AreaDescriptor

  def name          : String
  def originalImage : Array[Byte]

//  def descriptorChannels: Int
  def interestPoints: Map[Point, ADescriptor]
}


object ImageDescriptor{

  trait BetterSearch{
    self: ImageDescriptor =>

    type PointsGroupDescriptor = Double

    def pointsGroups: Map[PointsGroupDescriptor, Set[Point]]
  }

}


trait AreaDescriptor{
  type Channel <: ChannelDescriptor

  def sideLength: Int
  def channelsCount: Int

  def channels: Seq[Channel]
}

object AreaDescriptor{

  implicit class AreaDescriptorCompareWrapper[D <: ChannelDescriptor](ad: AreaDescriptor.SingleChannel{ type Channel = D })
                                                                     (implicit c: Comparator[D])
  {
    def canBeEqual(ad2: AreaDescriptor.SingleChannel{ type Channel = D }, precision: Double): Boolean =
      implicitly[Comparator[D]].canBeEqual(ad.channel, ad2.channel, precision)
  }

//  implicit class AreaDescriptorCompareWrapper[D  <: AreaDescriptor : Comparator](d: D){
//    def canBeEqual(d2: D, precision: Double): Boolean = implicitly[Comparator[D]].canBeEqual(d, d2, precision)
//  }


  trait SingleChannel extends AreaDescriptor{
    def channel: Channel

    final def channelsCount = 1
    final def channels      = channel :: Nil
  }

  trait HasStatistics extends AreaDescriptor{
    type Channel <: ChannelDescriptor with ChannelDescriptor.Statistics
  }



  // todo
}




trait ChannelDescriptor{
  def data  : Array[Double]
  def byRows: Array2D[Double]
  def byCols: Array2D[Double]
}



object ChannelDescriptor{

  implicit class ChannelDescriptorCompareWrapper[D  <: ChannelDescriptor : Comparator](d: D){
    def canBeEqual(d2: D, precision: Double): Boolean = implicitly[Comparator[D]].canBeEqual(d, d2, precision)
  }

  trait Comparator[D <: ChannelDescriptor]{
    def canBeEqual(d1: D, d2: D, precision: Double): Boolean
  }


  trait Statistics{
    self: ChannelDescriptor =>

    // for each channel

    def mean  : Double
    def std   : Double
    def range : Double
    /** interquartile range */
    def iqr   : Double
  }

  object ByIqrComparator extends Comparator[ChannelDescriptor with Statistics]{
    def canBeEqual(d1: ChannelDescriptor with Statistics, d2: ChannelDescriptor with Statistics, precision: Double): Boolean = ???
  }
}