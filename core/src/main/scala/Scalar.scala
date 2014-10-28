package scunits

import scunits.types._

case class Scalar[L <: Dims](v: Double) extends AnyVal with Ordered[Scalar[L]] {
  def +(m: Scalar[L]) = Scalar[L](v + m.v)
  def -(m: Scalar[L]) = Scalar[L](v - m.v)

  def *[R <: Dims, Qs <: QListOf[L#bases] with QListOf[R#bases]](r: Scalar[R])(implicit qs: Qs) = Scalar[qs.mult[L,R]](v * r.v)
  def /[R <: Dims, Qs <: QListOf[L#bases] with QListOf[R#bases]](r: Scalar[R])(implicit qs: Qs) = Scalar[qs.div[L,R]](v / r.v)

  def *(r: Double) = Scalar[L](v * r)
  def /(r: Double) = Scalar[L](v / r)

  def compare(that: Scalar[L]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Scalar[L]) = v == r.v

  def inv[Qs <: QListOf[L#bases]](implicit qs: Qs) = Scalar[qs.inv[L]](1.0 / v)

  def pow[Qs <: QListOf[L#bases], E <: Integer](e: E)(implicit qs: Qs) = Scalar[qs.pow[L,E]](v)
}