package scunits4

import scunits.BaseQuantityLike
import scunits.integer._
import scunits.integer.Ops._

trait Dim {
  type Quant <: BaseQuantity
  type Exp <: Integer
  type Mult[R <: DimOf[Quant]] = Quant ^ (Exp + R#Exp)
}
trait DimOf[Q <: BaseQuantity] extends Dim {
  type Quant = Q
}
trait ^[L <: BaseQuantity, R <: Integer] extends Dim {
  type Quant = L
  type Exp = R
}

trait DList {
  type Neg <: DList
}
trait DNil extends DList {
  type Neg = DNil
}
trait DNel extends DList with Dim {
  type Tail <: DList
  type Quant <: BaseQuantity
  type Exp <: Integer
  type Neg = (Quant ^ Exp#Neg) * Tail#Neg
}
trait DNelOf[Q <: BaseQuantity] extends DNel with DimOf[Q]
trait *[L <: Dim, R <: DList] extends DNel {
  type Tail = R
  type Quant = L#Quant
  type Exp = L#Exp
}

trait BaseQuantity extends DNel {
  type Quant <: BaseQuantity
  type Exp = _1
  type Tail = DNil
}
trait BaseQuantityOf[Q <: BaseQuantityOf[Q]] extends DNelOf[Q] with BaseQuantity

trait Simplifier[I <: DList] {
  type Out <: DList
}
trait SimplifierConst[I <: DList, O <: DList] extends Simplifier[I] {
  type Out = O
}

class Multer[L <: DList, R <: DList] {
  type Out <: DList
}
class MulterOf[L <: DList, R <: DList, O <: DList] extends Multer[L,R] {
  type Out = O
}
trait MulterSkip {
  // implicit def skip[L <: DNel, R <: Dim](implicit m: Multer[L#Tail,R])
}
trait MulterCancel {
  implicit def cancel[L <: DNel] = new MulterOf[L,L#Neg,DNil]
}
object Multer extends MulterSkip with MulterCancel {
  implicit def combine[L <: DNel, R <: DNelOf[L#Quant]] = new MulterOf[L,R,L#Mult[R] * R#Tail]

  implicit def dnilRight[L <: DList] = new MulterOf[L,DNil,L]
  implicit def dnilLeft[R <: DList] = new MulterOf[DNil,R,R]
}

case class Measure[D <: DList](v: Double) extends AnyVal {
  def ===(r: Measure[D]) = v == r.v
  def *[R <: DList](r: Measure[R])(implicit m: Multer[D,R]) = Measure[m.Out](v * r.v)
  def /[R <: DList](r: Measure[R])(implicit m: Multer[D,R#Neg]) = Measure[m.Out](v / r.v)
}

trait Length extends BaseQuantityOf[Length]
trait Mass extends BaseQuantityOf[Mass]
trait Time extends BaseQuantityOf[Time]

object Test {
  import Multer._
  def one[D <: DList] = Measure[D](1)

  one[Length] * one[DNil]: Measure[Length]
  one[DNil] * one[Length]: Measure[Length]
  one[Length] * one[Length]: Measure[Length ^ _2 * DNil]
  // one[Length].*(one[(Length ^ _1#Neg) * DNil])(cancel[Length])
}