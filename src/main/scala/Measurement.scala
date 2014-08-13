package scunits

protected case class Measurement[Q <: Quantity](v: Double) extends AnyVal {
  def +(m: Measurement[Q]) = Measurement(v + m.v)
  def -(m: Measurement[Q]) = Measurement(v - m.v)

  // def *[R <: Quantity, E <: Quantity](r: Measurement[R])(implicit m: Multiplier[Q,R,E]): Measurement[E] = m(this, r)
}