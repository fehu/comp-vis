package feh.tec.cvis.common

import org.opencv.core.Mat

import scala.reflect.ClassTag

object Helper{
  type Array2D[T] = Array[Array[T]]

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

    def toArray[R: ClassTag] = Helper.toArray(mat)
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
}
