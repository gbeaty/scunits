package scunits.types

import scunits._

object BQ {
  trait Length extends BaseQuantity {
    trait of { type length <: Integer }
    type set[To <: Integer] = of { type length = To }
    type get[L <: of] = L#length
  }
  trait Time extends BaseQuantity {
    trait of { type time <: Integer }
    type set[To <: Integer] = of { type time = To }
    type get[L <: of] = L#time
  }
  trait Info extends BaseQuantity {
    trait of { type info <: Integer }
    type set[To <: Integer] = of { type info = To }
    type get[L <: of] = L#info
  }
}

object Dims2Tests {
  // New stuff:
  implicit val bqs = new (BQ.Length :: BQ.Time :: BQ.Info :: QNil)

  type *[L <: Quant, R <: Quant] = bqs.mult[L,R]
  type /[L <: Quant, R <: Quant] = bqs.div[L,R]
  type ^[L <: BaseQuantity, R <: NonZeroInt] = L#to[R]

  type LengthMs = BQ.Length :: QNil
  type TimeMs = BQ.Time :: QNil
  type InfoMs = BQ.Info :: QNil

  type Length = BQ.Length ^ p1
  type Area = BQ.Length ^ p2
  type Volume = BQ.Length ^ p3
  type Time = BQ.Time ^ p1
  type Info = BQ.Info ^ p1
  type Frequency = BQ.Time ^ n1
  type Speed = (BQ.Length ^ p1) * (BQ.Time ^ n1)
  type BQel = Speed * Frequency
  type Bandwidth = (BQ.Info ^ p1) * (BQ.Time ^ n1)

  // Test append:
  type LengthTimeMs = LengthMs#append[TimeMs]
  implicitly[LengthTimeMs =:= (BQ.Length :: BQ.Time :: QNil)]
  implicitly[QNil#append[QNil] =:= QNil]
  implicitly[QNil#append[(BQ.Length :: QNil)] =:= (BQ.Length :: QNil)]
  implicitly[(BQ.Length :: QNil)#append[QNil] =:= (BQ.Length :: QNil)]

  // Test zeros:
  implicitly[QNil#zeros =:= Quant]
  implicitly[LengthTimeMs#zeros =:= BQ.Length#setQuant[BQ.Time#setQuant[Quant,_0],_0]]

  // Test setters/getters:
  implicitly[BQ.Length#get[BQ.Length#setQuant[Quant,p1]] =:= p1]
  implicitly[BQ.Time#get[BQ.Time#setQuant[Speed,n2]] =:= n2]
  implicitly[BQ.Time#get[BQ.Time#setQuant[BQ.Time#setQuant[Quant,n2],n1]] =:= n1]
  implicitly[BQ.Time#get[BQ.Time#setQuant[BQ.Time#setQuant[Quant,n1],n2]] =:= n2]
  implicitly[BQ.Time#get[TimeMs#zeros with BQ.Time#setQuant[BQ.Time#setQuant[Quant,n1],n2]] =:= n2]

  // Test negations:
  implicitly[bqs.neg[Dimless] =:= Dimless]
  implicitly[bqs.neg[Time] =:= Frequency]
  implicitly[bqs.neg[bqs.neg[Speed]] =:= Speed]
  implicitly[bqs.neg[Speed] =:= ((BQ.Length ^ n1) * (BQ.Time ^ p1))]

  // Test mults and divs
  implicitly[Dimless * Dimless =:= Dimless]
  implicitly[Dimless / Dimless =:= Dimless]
  implicitly[Time * Frequency =:= Dimless]
  implicitly[Volume / Area =:= Length]
  implicitly[Length * Length =:= Area]
  implicitly[Dimless / Time =:= Frequency]
  implicitly[Length / Time =:= Speed]    
  implicitly[Speed * Time =:= Length]
  implicitly[Frequency * Length =:= Speed]
  implicitly[Length * Frequency =:= Speed]
}