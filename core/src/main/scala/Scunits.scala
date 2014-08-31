package scunits

object Scunits {
  implicit def ordering[D <: Dims] = new Ordering[Measure[D]] {
    def compare(l: Measure[D], r: Measure[D]) = if(l < r) -1 else if(l > r) 1 else 0
  }
  implicit def cancelDenominator[A <: Dims, B <: Dims](m: Measure[A#Mult[B#Div[A]]]) = Measure[B](m.v)
  implicit def cancelNominator[A <: Dims, B <: Dims](m: Measure[A#Div[A#Div[B]]]) = Measure[B](m.v)
}