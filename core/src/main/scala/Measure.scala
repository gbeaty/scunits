package scunits

import scunits.types._

case class Measure[D <: Dims](v: Double) extends AnyVal {
  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: Dims, A <: Dims](r: Measure[R])(implicit mult: (Measure[D],Measure[R],Mult.type) => Measure[A]) = mult(this, r, Mult)
  def /[R <: Dims, A <: Dims](r: Measure[R])(implicit div: (Measure[D],Measure[R],Div.type) => Measure[A]) = div(this, r, Div)

  def *(r: Double) = Measure[D](v * r)
  def /(r: Double) = Measure[D](v / r)

  def >(r: Measure[D]) = v > r.v
  def >=(r: Measure[D]) = v >= r.v
  def <(r: Measure[D]) = v < r.v
  def <=(r: Measure[D]) = v <= r.v

  def ===(r: Measure[D]) = v == r.v

  def inv = Measure[D#Neg](1.0 / v)
}

case class Measure2[L <: Quant](v: Double) extends AnyVal {
  def +(m: Measure2[L]) = Measure2[L](v + m.v)
  def -(m: Measure2[L]) = Measure2[L](v - m.v)

  def *[R <: Quant, A <: QListOf[L with R]](r: Measure2[R])(implicit a: A) = Measure2[a.mult[L,R]](v * r.v)
  def /[R <: Quant, A <: QListOf[L with R]](r: Measure2[R])(implicit a: A) = Measure2[a.div[L,R]](v / r.v)

  def *(r: Double) = Measure2[L](v * r)
  def /(r: Double) = Measure2[L](v / r)

  def >(r: Measure2[L]) = v > r.v
  def >=(r: Measure2[L]) = v >= r.v
  def <(r: Measure2[L]) = v < r.v
  def <=(r: Measure2[L]) = v <= r.v

  def ===(r: Measure2[L]) = v == r.v

  def inv[A <: QListOf[L]](implicit a: A) = Measure2[a.neg[L]](1.0 / v)
}