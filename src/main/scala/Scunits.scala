package scunits

import scunits.tlist._
import scunits.integer._

class BaseQuantity(val name: String, val symbol: String)

trait Quantity[Q <: TEl[BaseQuantity], E <: TEl[Integer]] {
  type Quantities = Q
  type Exponents = E
  type Mult[R <: Quantity[Q,_ <: TEl[Integer]]] = Quantity[Q,E#Zip[R#Exponents,Integer]#Map[Op[+]]]
  type Div[R <: Quantity[Q,_ <: TEl[Integer]]] = Quantity[Q,E#Zip[R#Exponents,Integer]#Map[Op[-]]]
}

case class UnitM[Q](name: String, symbol: String, mult: Double = 1.0, offset: Double = 0.0) {

  def construct(v: Double) = Measure(v)

  def apply(v: Double) = construct(mult * v + offset)  
}

class Prefix(val name: String, val symbol: String, val mult: Double) {
  def apply[Q](u: UnitM[Q]) = UnitM[Q](name + u.name, symbol + u.symbol, mult * u.mult)
  def apply[Q](u: UnitM[Q], v: Double) = u.construct(mult * u.mult * v)
}