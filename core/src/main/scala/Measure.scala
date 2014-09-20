package scunits

case class Measure[D <: Dims](v: Double) extends AnyVal {

  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: DimsOf[D#Quants]](r: Measure[R]) = Measure[D#Mult[R]](r.v * v)
  def /[R <: DimsOf[D#Quants]](r: Measure[R]) = Measure[D#Div[R]](r.v / v)

  def ×(r: Double) = Measure[D](v * r)
  def ÷(r: Double) = Measure[D](v / r)

  def >(r: Measure[D]) = v > r.v
  def >=(r: Measure[D]) = v >= r.v
  def <(r: Measure[D]) = v < r.v
  def <=(r: Measure[D]) = v <= r.v

  def ===(r: Measure[D]) = v == r.v

  def inv = Measure[D#Neg](1.0 / v)
}