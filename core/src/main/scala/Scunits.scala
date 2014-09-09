package scunits

class Mult[L <: Dims, R, E <: Dims]
class Div[L <: Dims, R, E <: Dims]

trait LowPriorityImplicits {
  object Mult
  object Div

  implicit def multDims[L <: Dims, R <: Dims] = new Mult[L,R,L#Mult[R]]
  implicit def divDims[L <: Dims, R <: Dims] = new Div[L,R,L#Div[R]]
}

object Scunits extends LowPriorityImplicits {
  implicit def ordering[D <: Dims] = new Ordering[Measure[D]] {
    def compare(l: Measure[D], r: Measure[D]) = if(l < r) -1 else if(l > r) 1 else 0
  }

  implicit def multDNil[L <: Dims] = new Mult[L,DNil,L]
  implicit def multDenomR[L <: Dims,R <: Dims] = new Mult[L,R#Div[L],R]
  implicit def multDenomL[L <: Dims,R <: Dims] = new Mult[L#Div[R],R,L]

  implicit def divDNil[L <: Dims,R <: Dims] = new Div[L,DNil,L]
  implicit def divDenom[L <: Dims,R <: Dims] = new Div[L,L#Div[R],R]
  implicit def divNum[L <: Dims,R <: Dims] = new Div[R#Div[L],R,L#Neg]
  implicit def divSelf[L <: Dims] = new Div[L,L,DNil]
  
  val coef = UnitM[DNil]("","")
}