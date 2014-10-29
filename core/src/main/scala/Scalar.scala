package scunits

import scunits.types._

case class Box[A]() {
  type dims = A
}

case class Scalar[L <: Dims](v: Double) extends AnyVal with Ordered[Scalar[L]] {
  type dims = DimsConst[L#qlist, L#values]

  def +(m: Scalar[L]) = Scalar[L](v + m.v)
  def -(m: Scalar[L]) = Scalar[L](v - m.v)

  def *[R <: DimsOf[L#qlist]](r: Scalar[R]) = Scalar[dims#op[R]#mult](v * r.v)
  def /[R <: DimsOf[L#qlist]](r: Scalar[R]) = Scalar[dims#op[R]#div](v / r.v)

  def ×(r: Double) = Scalar[L](v * r)
  def ÷(r: Double) = Scalar[L](v / r)

  def compare(that: Scalar[L]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Scalar[L]) = v == r.v

  def inv = Scalar[dims#inv](1.0 / v)
}