package scunits

import scunits.types._

case class Scalar[L <: Dims](v: Double) extends AnyVal {
  def +(m: Scalar[L]) = Scalar[L](v + m.v)
  def -(m: Scalar[L]) = Scalar[L](v - m.v)

  def *[R <: Dims, A <: QListOf[L with R]](r: Scalar[R])(implicit a: A) = Scalar[a.mult[L,R]](v * r.v)
  def /[R <: Dims, A <: QListOf[L with R]](r: Scalar[R])(implicit a: A) = Scalar[a.div[L,R]](v / r.v)

  def *(r: Double) = Scalar[L](v * r)
  def /(r: Double) = Scalar[L](v / r)

  def >(r: Scalar[L]) = v > r.v
  def >=(r: Scalar[L]) = v >= r.v
  def <(r: Scalar[L]) = v < r.v
  def <=(r: Scalar[L]) = v <= r.v

  def ===(r: Scalar[L]) = v == r.v

  def inv[A <: QListOf[L]](implicit a: A) = Scalar[a.neg[L]](1.0 / v)
}