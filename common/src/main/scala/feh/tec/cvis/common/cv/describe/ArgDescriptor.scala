package feh.tec.cvis.common.cv.describe

import scala.reflect.ClassTag

case class ArgDescriptor[T](name: String, description: String, modifiers: ArgModifier[T]*)
                           (implicit val tag: ClassTag[T])
{
  def &:(mod: ArgModifier[T]) = ArgDescriptor(name, description, mod +: modifiers: _*)

  override def equals(obj: scala.Any): Boolean = canEqual(obj) && (obj match {
    case that: ArgDescriptor[_] => this.name == that.name && this.tag == that.tag
  })
}

trait ArgModifier[T]{
//  def test(v: T): Boolean
}

object ArgModifier{
  trait Positive[T] extends ArgModifier[T]
  trait NonNegative[T] extends ArgModifier[T]
  trait Integer[T] extends ArgModifier[T]

  case class MaxCap[T: Ordering](max: T) extends ArgModifier[T]{
    def test(v: T) = implicitly[Ordering[T]].lteq(v, max)
  }
  case class MinCap[T: Ordering](min: T) extends ArgModifier[T]{
    def test(v: T) = implicitly[Ordering[T]].gteq(v, min)
  }

  trait NotEmpty[T] extends ArgModifier[T]

  /** just a marker */
  trait Optional[T] extends ArgModifier[T]

  def Positive   [T](implicit pos: Positive[T])     = pos
  def NonNegative[T](implicit nneg: NonNegative[T]) = nneg

  def NotEmpty   [T](implicit ne: NotEmpty[T])      = ne

  def Optional   [T](implicit opt: Optional[T])     = opt

  implicit def numericCanBePositive   [N](implicit num: Numeric[N]): Positive[N]     = new Positive[N] {
    def test(v: N) = num.gt(v, num.zero)
  }
  implicit def numericCanBeNonNegative[N](implicit num: Numeric[N]): NonNegative[N]  = new NonNegative[N] {
    def test(v: N) = num.gteq(v, num.zero)
  }
  implicit def numericCanBeInteger    [N: Integral]                : Integer[N]      = new Integer[N] {
    def test(v: N) = true
  }

  implicit def iterableCanBeEmpty[I <: Iterable[_]]                : NotEmpty[I] = new NotEmpty[I] {
    def test(v: I) = v.nonEmpty
  }

  implicit def anyCanBeMarkedOptional[T]: Optional[T] = new Optional[T]{  }
}