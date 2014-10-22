package scunits

import scunits.types._

import scala.math.BigDecimal

case class UnitM[L <: Dims](
  name: Option[String] = None,
  symbol: Option[String] = None,
  mult: BigDecimal = 1.0,
  offset: BigDecimal = 0.0,
  prefix: Option[UnitPrefix] = None) {

  val prefixedMult: BigDecimal = prefix.map(_.mult * mult).getOrElse(mult)
  val prefixedMultDouble = prefixedMult.toDouble
  val doubleOffset = offset.toDouble
  
  def apply(in: Double) = Scalar[L](prefixedMultDouble * (in + doubleOffset))
  def unapply(out: Scalar[L]) = out.v / prefixedMultDouble - doubleOffset

  def label(n: String, s: String) = copy[L](name = Some(n), symbol = Some(s))
  def rename(n: String) = copy[L](name = Some(n))

  def inv[A <: QListOf[L]](implicit a: A) = UnitM[a.neg[L]](mult = 1 / mult)

  def *[R <: Dims, A <: QListOf[L with R]](r: UnitM[R]) = UnitM[A#mult[L,R]](mult = prefixedMult * r.prefixedMult)
  def /[R <: Dims, A <: QListOf[L with R]](r: UnitM[R]) = UnitM[A#div[L,R]](mult = prefixedMult / r.prefixedMult)

  def *(r: BigDecimal) = UnitM[L](mult = prefixedMult * r)
  def *(r: Double) = UnitM[L](mult = prefixedMult * r)
  def *(r: Float) = UnitM[L](mult = prefixedMult * r)
  def *(r: Long) = UnitM[L](mult = prefixedMult * r)
  def *(r: Int) = UnitM[L](mult = prefixedMult * r)
  def *(r: Short) = UnitM[L](mult = prefixedMult * r)
  def *(r: Byte) = UnitM[L](mult = prefixedMult * r)

  def /(r: BigDecimal) = UnitM[L](mult = prefixedMult / r)
  def /(r: Double) = UnitM[L](mult = prefixedMult / r)
  def /(r: Float) = UnitM[L](mult = prefixedMult / r)
  def /(r: Long) = UnitM[L](mult = prefixedMult / r)
  def /(r: Int) = UnitM[L](mult = prefixedMult / r)
  def /(r: Short) = UnitM[L](mult = prefixedMult / r)
  def /(r: Byte) = UnitM[L](mult = prefixedMult / r)

  def prefixLabel(pn: String, ps: String) = (name, symbol) match {
      case (Some(n),Some(s)) => this.label(pn + n, ps + s)
      case _ => this
    }
  def sq = (this * this).prefixLabel("square ", "sq-")
  def cu = (this * this * this).prefixLabel("cubic", "cu-")
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