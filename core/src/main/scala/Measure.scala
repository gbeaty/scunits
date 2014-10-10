package scunits

import scunits.types._

case class Measure[D <: Dims](v: Double) extends AnyVal with Ordered[Measure[D]] {
  type dims = D
  // type Conv[RD <: Dims] = ConverterFromTo[D#quants,RD#quants] with CachedConverter

  def +(m: Measure[D]) = Measure[D](v + m.v)  
  def -(m: Measure[D]) = Measure[D](v - m.v)
  // def +[RD <: Dims, C <: Conv[RD]](m: Measure[C#apply[D]])(implicit c: C) = Measure[c.apply[D]](v + m.v)
  // def -[RD <: Dims, C <: Conv[RD]](m: Measure[C#apply[D]])(implicit c: C) = Measure[c.apply[D]](v - m.v)  

  def *[R <: DimsOf[D#quants]](r: Measure[R]) = Measure[D#mult[R]](v * r.v)  
  def /[R <: DimsOf[D#quants]](r: Measure[R]) = Measure[D#div[R]](v / r.v)
  // def *[RD <: Dims, C <: Conv[RD]](m: Measure[RD])(implicit c: C) = Measure[c.apply[D]#mult[RD]](v * m.v)
  // def /[RD <: Dims, C <: Conv[RD]](m: Measure[RD])(implicit c: C) = Measure[c.apply[D]#mult[RD]](v * m.v)

  def ×(r: Double) = Measure[D](v * r)
  def ÷(r: Double) = Measure[D](v / r)
  def mult(r: Double) = Measure[D](v * r)
  def div(r: Double) = Measure[D](v / r)

  def compare(that: Measure[D]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Measure[D]) = v == r.v

  def inv = Measure[D#neg](1.0 / v)
}