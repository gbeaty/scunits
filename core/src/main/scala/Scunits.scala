package scunits

class Mult[L <: Dims, R, E <: Dims]
class Div[L <: Dims, R, E <: Dims]

trait LowPriorityImplicits {
  object Mult
  object Div
  implicit def multDims[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R], op: Mult.type) = Measure[L#Mult[R]](l.v * r.v)
  implicit def divDims[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R], op: Div.type) = Measure[L#Div[R]](l.v / r.v)

  implicit def _multDims[L <: Dims, R <: Dims] = new Mult[L,R,L#Mult[R]]
  implicit def _divDims[L <: Dims, R <: Dims] = new Div[L,R,L#Div[R]]
}

object Scunits extends LowPriorityImplicits {
  implicit def ordering[D <: Dims] = new Ordering[Measure[D]] {
    def compare(l: Measure[D], r: Measure[D]) = if(l < r) -1 else if(l > r) 1 else 0
  }
    
  implicit def multDNil[L <: Dims](l: Measure[L], r: Measure[DNil], op: Mult.type) = Measure[L](l.v * r.v)
  implicit def multDenomR[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R#Div[L]], op: Mult.type) = Measure[R](l.v * r.v)
  implicit def multDenomL[L <: Dims,R <: Dims](l: Measure[L#Div[R]], r: Measure[R], op: Mult.type) = Measure[L](l.v * r.v)

  implicit def divDNil[L <: Dims,R <: Dims](l: Measure[L], r: Measure[DNil], op: Div.type) = Measure[L](l.v / r.v)
  implicit def divDenom[L <: Dims,R <: Dims](l: Measure[L], r: Measure[L#Div[R]], op: Div.type) = Measure[R](l.v / r.v)
  implicit def divNum[L <: Dims,R <: Dims](l: Measure[R#Div[L]], r: Measure[R], op: Div.type) = Measure[L#Neg](l.v / r.v)
  implicit def divSelf[L <: Dims](l: Measure[L], r: Measure[L], op: Div.type) = Measure[DNil](l.v / r.v)

  implicit def _multDNil[L <: Dims] = new Mult[L,DNil,L]
  implicit def _multDenomR[L <: Dims,R <: Dims] = new Mult[L,R#Div[L],R]
  implicit def _multDenomL[L <: Dims,R <: Dims] = new Mult[L#Div[R],R,L]

  implicit def _divDNil[L <: Dims,R <: Dims] = new Div[L,DNil,L]
  implicit def _divDenom[L <: Dims,R <: Dims] = new Div[L,L#Div[R],R]
  implicit def _divNum[L <: Dims,R <: Dims] = new Div[R#Div[L],R,L#Neg]
  implicit def _divSelf[L <: Dims] = new Div[L,L,DNil]
  
  val coef = UnitM[DNil]("","")
}