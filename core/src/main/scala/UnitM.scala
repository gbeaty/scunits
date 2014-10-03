package scunits

import scunits.integer._

import scala.math._

case class UnitM[D <: Dims](
  name: Option[String] = None,
  symbol: Option[String] = None,
  mult: BigDecimal = 1.0,
  offset: BigDecimal = 0.0,
  prefix: Option[UnitPrefix] = None) {

  val prefixedMult: BigDecimal = prefix.map(_.mult * mult).getOrElse(mult)
  val prefixedMultDouble = prefixedMult.toDouble
  val doubleOffset = offset.toDouble
  
  def apply(in: Double) = Measure[D](prefixedMultDouble * (in + doubleOffset))
  def unapply(out: Measure[D]) = out.v / prefixedMultDouble - doubleOffset

  // def apply(in: BigDecimal) = Measure[D](prefixedMult * in + offset)
  // def unapply(out: Measure[D]) = (out.v - doubleOffset) / prefixedMultDouble

  def label(n: String, s: String) = copy[D](name = Some(n), symbol = Some(s))
  def rename(n: String) = copy[D](name = Some(n))

  def inv = UnitM[D#Neg](mult = 1 / mult)

  def *[R <: Dims](r: UnitM[R])(implicit m: Multer[D,R]) = UnitM[m.Out](mult = prefixedMult * r.prefixedMult)
  def /[R <: Dims](r: UnitM[R])(implicit m: Multer[D,R#Neg]) = UnitM[m.Out](mult = prefixedMult / r.prefixedMult)

  def *(r: BigDecimal) = UnitM[D](mult = prefixedMult * r)
  def *(r: Double) = UnitM[D](mult = prefixedMult * r)
  def *(r: Float) = UnitM[D](mult = prefixedMult * r)
  def *(r: Long) = UnitM[D](mult = prefixedMult * r)
  def *(r: Int) = UnitM[D](mult = prefixedMult * r)
  def *(r: Short) = UnitM[D](mult = prefixedMult * r)
  def *(r: Byte) = UnitM[D](mult = prefixedMult * r)

  def /(r: BigDecimal) = UnitM[D](mult = prefixedMult / r)
  def /(r: Double) = UnitM[D](mult = prefixedMult / r)
  def /(r: Float) = UnitM[D](mult = prefixedMult / r)
  def /(r: Long) = UnitM[D](mult = prefixedMult / r)
  def /(r: Int) = UnitM[D](mult = prefixedMult / r)
  def /(r: Short) = UnitM[D](mult = prefixedMult / r)
  def /(r: Byte) = UnitM[D](mult = prefixedMult / r)

  def prefixLabel(pn: String, ps: String) = (name, symbol) match {
    case (Some(n),Some(s)) => this.label(pn + n, ps + s)
    case _ => this
  }
  def sq = UnitM[D#Pow[p2]](mult = prefixedMult.pow(2)).prefixLabel("square", "sq-")
  def cu = UnitM[D#Pow[p3]](mult = prefixedMult.pow(3)).prefixLabel("cubic", "cu-")
}
object UnitM {
  def apply[D <: Dims](name: String, symbol: String): UnitM[D] =
    UnitM[D](Some(name), Some(symbol), 1.0)

  def apply[D <: Dims](name: String, symbol: String, mult: BigDecimal): UnitM[D] =
    UnitM[D](Some(name), Some(symbol), mult)

  def apply[D <: Dims](name: String, symbol: String, mult: BigDecimal, offset: BigDecimal): UnitM[D] =
    UnitM[D](Some(name), Some(symbol), mult, offset)
}

case class UnitPrefix(namePrefix: String, symbolPrefix: String, mult: BigDecimal) {
  val doubleMult = mult.toDouble
  def apply[D <: Dims](u: UnitM[D]) = u.copy[D](
    name = u.name.map(namePrefix + _),
    symbol = u.symbol.map(symbolPrefix + _),
    prefix = Some(this)
  )

  def apply[D <: Dims](u: UnitM[D], v: Double) = u(doubleMult * v)
}