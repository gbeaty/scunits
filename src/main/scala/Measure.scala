package scunits

import scunits.tlist._
import scunits.integer._
import scunits.quantity._

protected case class Measure[Q <: TEl[BaseQuantity], E <: TEl[Integer]](v: Double) extends AnyVal {
  type Dim = Dimension[Q,E]
  def +(m: Measure[Q,E]) = Measure[Q,E](v + m.v)
  def -(m: Measure[Q,E]) = Measure[Q,E](v - m.v)

  def *[RE <: TEl[Integer]](r: Measure[Q,RE]) = Measure[Q,Dim#Mult[r.Dim]#Exponents](v * r.v)
  def /[RE <: TEl[Integer]](r: Measure[Q,RE]) = Measure[Q,Dim#Div[r.Dim]#Exponents](v * r.v)
}