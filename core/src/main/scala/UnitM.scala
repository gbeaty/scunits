package scunits

import scunits.quantity._
import scunits.integer._
import scunits.integer.Ops._

case class UnitM[D <: Dims](
  name: Option[String] = None,
  symbol: Option[String] = None,
  mult: Double = 1.0,
  offset: Double = 0.0,
  prefix: Option[Prefix] = None) {

  val prefixedMult = prefix.map(_.mult).getOrElse(1.0) * mult
  
  def apply(in: Double) = Measure[D](prefixedMult * in + offset)
  def unapply(out: Measure[D]) = (out.v - offset) / prefixedMult

  def label(n: String, s: String) = copy[D](name = Some(n), symbol = Some(s))
  def *[R <: Dims](r: UnitM[R]) = UnitM[D#Mult[R]](mult = prefixedMult * r.prefixedMult)
  def /[R <: Dims](r: UnitM[R]) = UnitM[D#Div[R]](mult = prefixedMult / r.prefixedMult)
}
object UnitM {
  def apply[D <: Dims](name: String, symbol: String): UnitM[D] =
    UnitM[D](Some(name), Some(symbol), 1.0)

  def apply[D <: Dims](name: String, symbol: String, mult: Double): UnitM[D] =
    UnitM[D](Some(name), Some(symbol), mult)

  def apply[D <: Dims](name: String, symbol: String, mult: Double, offset: Double): UnitM[D] =
    UnitM[D](Some(name), Some(symbol), mult, offset)
}

case class Prefix(namePrefix: String, symbolPrefix: String, mult: Double) {
  def apply[D <: Dims](u: UnitM[D]) = u.copy[D](
    name = u.name.map(namePrefix + _),
    symbol = u.symbol.map(symbolPrefix + _),
    prefix = Some(this)
  )

  def apply[D <: Dims](u: UnitM[D], v: Double) = u(mult * v)
}