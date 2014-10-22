package scunits

import scunits.types._

case class Measure[L <: Dims](v: Double) extends AnyVal {
  def +(m: Measure[L]) = Measure[L](v + m.v)
  def -(m: Measure[L]) = Measure[L](v - m.v)

  def *[R <: Dims, A <: QListOf[L with R]](r: Measure[R])(implicit a: A) = Measure[a.mult[L,R]](v * r.v)
  def /[R <: Dims, A <: QListOf[L with R]](r: Measure[R])(implicit a: A) = Measure[a.div[L,R]](v / r.v)

  def *(r: Double) = Measure[L](v * r)
  def /(r: Double) = Measure[L](v / r)

  def >(r: Measure[L]) = v > r.v
  def >=(r: Measure[L]) = v >= r.v
  def <(r: Measure[L]) = v < r.v
  def <=(r: Measure[L]) = v <= r.v

  def ===(r: Measure[L]) = v == r.v

  def inv[A <: QListOf[L]](implicit a: A) = Measure[a.neg[L]](1.0 / v)
}