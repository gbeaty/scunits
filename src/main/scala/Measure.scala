package scunits

import scunits.tlist._
import scunits.integer._
import scunits.quantity._

protected case class Measure[Q <: TEl[BaseQuantity], E <: TEl[Integer]](v: Double) extends AnyVal {
  type Quant = Dimensions[Q,E]
  def +(m: Measure[Q,E]) = Measure[Q,E](v + m.v)
  def -(m: Measure[Q,E]) = Measure[Q,E](v - m.v)

  def *[RE <: TEl[Integer]](r: Measure[Q,RE]) = Measure[Q,Quant#Mult[r.Quant]#Mags](v * r.v)
  def /[RE <: TEl[Integer]](r: Measure[Q,RE]) = Measure[Q,Quant#Div[r.Quant]#Mags](v * r.v)
}