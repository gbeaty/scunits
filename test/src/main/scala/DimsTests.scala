package scunits.types

import scunits._

object DimTests {
  implicit val bqs = scunits.siBaseQuantities
  import bqs._

  type LengthMs = Basis.Length :: QNil
  type TimeMs = Basis.Time :: QNil
  type InfoMs = Basis.Info :: QNil

  type Length = Basis.Length ^ p1
  type Area = Basis.Length ^ p2
  type Volume = Basis.Length ^ p3
  type Time = Basis.Time ^ p1
  type Info = Basis.Info ^ p1
  type Frequency = Basis.Time ^ n1
  type Speed = (Basis.Length ^ p1)#op[(Basis.Time ^ n1)]#mult
  type BQel = Speed#op[Frequency]#mult
  type Bandwidth = (Basis.Info ^ p1)#op[(Basis.Time ^ n1)]#mult

  // Test append:
  type LengthTimeMs = LengthMs#append[TimeMs]
  implicitly[LengthTimeMs =:= (Basis.Length :: Basis.Time :: QNil)]
  implicitly[QNil#append[QNil] =:= QNil]
  implicitly[QNil#append[(Basis.Length :: QNil)] =:= (Basis.Length :: QNil)]
  implicitly[(Basis.Length :: QNil)#append[QNil] =:= (Basis.Length :: QNil)]

  // Test zeros:
  implicitly[QNil#zeros =:= Dim]
  implicitly[LengthTimeMs#zeros =:= Basis.Length#set[_0] with Basis.Time#set[_0] with Dim]

  // Test negations:
  implicitly[Dimless#inv =:= Dimless]
  implicitly[Time#inv =:= Frequency]
  implicitly[Speed#inv#inv =:= Speed]
  implicitly[Speed#inv =:= (Basis.Length ^ n1)#op[Basis.Time ^ p1]#mult]

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