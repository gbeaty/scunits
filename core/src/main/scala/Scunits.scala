package scunits

trait LowPriorityImplicits {
  object Mult
  object Div
  implicit def multDims[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R], op: Mult.type) = Measure[L#Mult[R]](l.v * r.v)
  implicit def divDims[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R], op: Div.type) = Measure[L#Div[R]](l.v / r.v)
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

  implicit def invert[F <: Dims](f: Measure[F]) = Measure[F#Neg](1.0 / f.v)
  
  val coef = UnitM[DNil]("","")

  implicit def toCoef(d: Double) = Measure[DNil](d)
}