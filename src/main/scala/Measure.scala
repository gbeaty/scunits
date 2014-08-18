package scunits

import scunits.tlist._
import scunits.integer._

/*protected case class Measure[Q <: TEl[BaseQuantity], E <: TEl[Integer]](v: Double) extends AnyVal {
  type Quant = Quantity[Q,E]
  def +(m: Measure[Q,E]) = Measure[Q,E](v + m.v)
  def -(m: Measure[Q,E]) = Measure[Q,E](v - m.v)

  def *[RE <: TEl[Integer]](r: Measure[Q,RE]) = Measure[Q,Quant#Mult[r.Quant]#Exponents](v * r.v)
  def /[RE <: TEl[Integer]](r: Measure[Q,RE]) = Measure[Q,Quant#Div[r.Quant]#Exponents](v * r.v)
}*/