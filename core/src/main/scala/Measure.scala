package scunits

import scunits.integer._

case class Measure[D <: Dims](v: Double) extends AnyVal {
  def *[R <: Dims](r: Measure[R])(implicit m: Multer[D,R]) = Measure[m.Out](v * r.v)
  def /[R <: Dims](r: Measure[R])(implicit m: Multer[D,R#Neg]) = Measure[m.Out](v / r.v)
  
  def +[R <: Dims](r: Measure[R])(implicit a: Additive[D#Set,R#Set]) = Measure[D](v + r.v)
  def -[R <: Dims](r: Measure[R])(implicit a: Additive[D#Set,R#Set]) = Measure[D](v - r.v)

  def ×(r: Double) = Measure[D](v * r)
  def ÷(r: Double) = Measure[D](v / r)

  def >[R <: Dims](r: Measure[D])(implicit a: Additive[D#Set,R#Set]) = v > r.v
  def >=[R <: Dims](r: Measure[D])(implicit a: Additive[D#Set,R#Set]) = v >= r.v
  def <[R <: Dims](r: Measure[D])(implicit a: Additive[D#Set,R#Set]) = v < r.v
  def <=[R <: Dims](r: Measure[D])(implicit a: Additive[D#Set,R#Set]) = v <= r.v

  def ===[R <: Dims](r: Measure[D])(implicit a: Additive[D#Set,R#Set]) = v == r.v

  def inv = Measure[D#Neg](1.0 / v)
}