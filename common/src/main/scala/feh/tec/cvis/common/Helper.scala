package feh.tec.cvis.common

import org.opencv.core._
import feh.util._
import scala.math.Numeric.{FloatIsFractional, IntIsIntegral, DoubleIsFractional}
import scala.reflect.ClassTag
import scala.language.implicitConversions

object Helper{
  type Array2D[T] = Array[Array[T]]

  implicit def pairIsCvPoint[N](p: (N, N))(implicit num: Numeric[N]): Point = new Point(num.toDouble(p._1), num.toDouble(p._2))
  implicit def pairIsCvSize[N] (p: (N, N))(implicit num: Numeric[N]): Size  = new Size (num.toDouble(p._1), num.toDouble(p._2))

  implicit def numericArrayIsCvPoint[N](arr: Array[N])(implicit num: Numeric[N]): Point = new Point(arr.map(num.toDouble))
  
  implicit def cvPointIsDoublePair(p: Point): (Double, Double) = p.x      -> p.y
  implicit def cvSizeIsDoublePair (s: Size):  (Double, Double) = s.width  -> s.height

  def withMat[R](f: Mat => R) = f(new Mat())

  def toArray[T: ClassTag](mat: Mat) = {
    val arr = Array.ofDim[T](mat.width * mat.height * mat.channels)
    implicitly[ClassTag[T]] match {
      case ClassTag.Byte    => mat.get(0, 0, arr.asInstanceOf[Array[Byte]])
      case ClassTag.Short   => mat.get(0, 0, arr.asInstanceOf[Array[Short]])
      case ClassTag.Int     => mat.get(0, 0, arr.asInstanceOf[Array[Int]])
      case ClassTag.Float   => mat.get(0, 0, arr.asInstanceOf[Array[Float]])
      case ClassTag.Double  => mat.get(0, 0, arr.asInstanceOf[Array[Double]])
    }
    arr
  }

  def mapMat[R: ClassTag](mat: Mat, f: (Int, Int) => Array[Double] => R): Array2D[R] = {
    val arr = Array.ofDim[R](mat.rows, mat.cols)
    for{
      i <- 0 until mat.rows
      j <- 0 until mat.cols
      v = mat.get(i, j)
    } arr(i)(j) = f(i, j)(v)
    arr
  }

  implicit class MatWrapper(mat: Mat){
    def map[R: ClassTag](f: (Int, Int) => Array[Double] => R) = mapMat(mat, f)
    def mapV[R: ClassTag](f: Array[Double] => R) = mapMat(mat, (_, _) => f)

    def byRow[R: ClassTag](f: Int => Mat => R): Stream[R] =
      for{
        i <- Stream from 0 take mat.rows()
        row = mat.row(i)
      } yield f(i)(row)

    def toArray[R: ClassTag]: Array[R] = Helper.toArray(mat)

    // C++: void Mat::convertTo(Mat& m, int rtype, double alpha = 1, double beta = 0)
    def convert(rtype: Int, alpha: Double = 1, beta: Double = 0) = withMat(_ $$ (mat.convertTo(_, rtype, alpha, beta)))
  }

  implicit class Array2DWrapper[T](arr: Array2D[T]){
    lazy val rows = arr.length
    lazy val cols = if(rows > 0) arr(0).length else 0

    def size = rows * cols

    def map[R: ClassTag](f: (Int, Int) => T => R): Array2D[R] = {
      val res = Array.ofDim[R](rows, cols)
      for{
        i <- 0 until rows
        j <- 0 until cols
        v = arr(i)(j)
      } res(i)(j) = f(i, j)(v)
      res
    }

    def lazyPairs: Stream[((Int, Int), T)] = Stream.from(0).take(rows).flatMap(
      i => Stream.from(0).take(cols).map{
        j =>
          (i, j) -> arr(i)(j)
      }
    )
  }

  implicit class LazyNumericPairsWrapper[N: Numeric](stream: Stream[((Int, Int), N)]){
    lazy val num = implicitly[Numeric[N]]

    def toMat(rows: Int, cols: Int): Mat = new Mat(rows, cols, cvTpe) $$ {
      mat =>
        stream.foreach {
          case ((i, j), v) => mat.put(i, j, num.toDouble(v))
        }
    }
    
    def toMatOfPoint: MatOfPoint = new MatOfPoint(stream.map(_._1: Point): _*)

    def cvTpe = num match {
      case _: DoubleIsFractional  => CvType.CV_64F
      case _: FloatIsFractional   => CvType.CV_32F
      case _: IntIsIntegral       => CvType.CV_32S
   // todo
    }
  }
}
