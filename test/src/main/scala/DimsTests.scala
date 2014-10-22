package scunits.types

import scunits._

object DimsTests {
  // New stuff:
  // implicit val bqs = new (Basis.Length :: Basis.Time :: Basis.Info :: QNil)
  implicit val bqs = scunits.siBaseQuantities
  import bqs.ops._

  type ^[L <: BaseQuantity, R <: NonZeroInt] = L#to[R]

  type LengthMs = Basis.Length :: QNil
  type TimeMs = Basis.Time :: QNil
  type InfoMs = Basis.Info :: QNil

  type Length = Basis.Length ^ p1
  type Area = Basis.Length ^ p2
  type Volume = Basis.Length ^ p3
  type Time = Basis.Time ^ p1
  type Info = Basis.Info ^ p1
  type Frequency = Basis.Time ^ n1
  type Speed = (Basis.Length ^ p1) * (Basis.Time ^ n1)
  type BQel = Speed * Frequency
  type Bandwidth = (Basis.Info ^ p1) * (Basis.Time ^ n1)

  // Test append:
  type LengthTimeMs = LengthMs#append[TimeMs]
  implicitly[LengthTimeMs =:= (Basis.Length :: Basis.Time :: QNil)]
  implicitly[QNil#append[QNil] =:= QNil]
  implicitly[QNil#append[(Basis.Length :: QNil)] =:= (Basis.Length :: QNil)]
  implicitly[(Basis.Length :: QNil)#append[QNil] =:= (Basis.Length :: QNil)]

  // Test zeros:
  implicitly[QNil#zeros =:= Dims]
  implicitly[LengthTimeMs#zeros =:= Basis.Length#setDim[Basis.Time#setDim[Dims,_0],_0]]

  // Test setters/getters:
  implicitly[Basis.Length#get[Basis.Length#setDim[Dims,p1]] =:= p1]
  implicitly[Basis.Time#get[Basis.Time#setDim[Speed,n2]] =:= n2]
  implicitly[Basis.Time#get[Basis.Time#setDim[Basis.Time#setDim[Dims,n2],n1]] =:= n1]
  implicitly[Basis.Time#get[Basis.Time#setDim[Basis.Time#setDim[Dims,n1],n2]] =:= n2]
  implicitly[Basis.Time#get[TimeMs#zeros with Basis.Time#setDim[Basis.Time#setDim[Dims,n1],n2]] =:= n2]

  // Test negations:
  implicitly[bqs.neg[Dimless] =:= Dimless]
  implicitly[bqs.neg[Time] =:= Frequency]
  implicitly[bqs.neg[bqs.neg[Speed]] =:= Speed]
  implicitly[bqs.neg[Speed] =:= ((Basis.Length ^ n1) * (Basis.Time ^ p1))]

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