package scunits

import scunits.types._

case class Scalar[D <: Dims](v: Double) extends AnyVal with Ordered[Scalar[D]] {
  type dims = D

  def +(m: Scalar[D]) = Scalar[D](v + m.v)  
  def -(m: Scalar[D]) = Scalar[D](v - m.v)

  def *[R <: DimsOf[D#quants]](r: Scalar[R]) = Scalar[D#mult[R]](v * r.v)  
  def /[R <: DimsOf[D#quants]](r: Scalar[R]) = Scalar[D#div[R]](v / r.v)

  def ×(r: Double) = Scalar[D](v * r)
  def ÷(r: Double) = Scalar[D](v / r)
  def mult(r: Double) = Scalar[D](v * r)
  def div(r: Double) = Scalar[D](v / r)

  def compare(that: Scalar[D]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Scalar[D]) = v == r.v

  def inv = Scalar[D#neg](1.0 / v)
}