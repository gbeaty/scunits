package scunits.types

object BQ {
  trait Length extends Quant { type length <: Integer }
  trait Time extends Quant { type time <: Integer }
  trait Info extends Quant { type info <: Integer }
}
object Accessor {
  trait Length extends AccessorOf[Length, BQ.Length] {
    type set[L <: Quant, To <: Integer] = L with BQ.Length { type length = To }
    type get[L <: of] = L#length
  }
  trait Time extends AccessorOf[Time, BQ.Time] {
    type set[L <: Quant, To <: Integer] = L with BQ.Time { type time = To }
    type get[L <: of] = L#time
  }
  trait Info extends AccessorOf[Info, BQ.Info] {
    type set[L <: Quant, To <: Integer] = L with BQ.Info { type info = To }
    type get[L <: of] = L#info
  }
}

object DimsTests {
  type ^[L <: Accessor, R <: Integer] = L#set[Quant,R]
  type LengthMs = Accessor.Length :: ANil
  type TimeMs = Accessor.Time :: ANil
  type InfoMs = Accessor.Info :: ANil

  type Length = Accessor.Length#to[p1]
  type Area = Accessor.Length#to[p2]
  type Volume = Accessor.Length#to[p3]
  type Time = Accessor.Time#to[p1]
  type Info = Accessor.Info#to[p1]
  type Frequency = DimsConst[Accessor.Time :: ANil, Accessor.Time#set[Quant, n1]]
  type Speed = DimsConst[Accessor.Length :: Accessor.Time :: ANil, Accessor.Time#set[Accessor.Length#set[Quant, p1], n1]]
  type Accel = DimsConst[Accessor.Length :: Accessor.Time :: ANil, Accessor.Time#set[Accessor.Length#set[Quant, p1], n2]]
  type Bandwidth = DimsConst[Accessor.Info :: Accessor.Time :: ANil, Accessor.Time#set[Accessor.Info#set[Quant, p1], n1]]

  // Test base:
  implicitly[Time =:= DimsConst[Accessor.Time :: ANil, Quant with BQ.Time { type time = p1 }]]

  // Test append:
  type LengthTimeMs = LengthMs#append[TimeMs]
  implicitly[LengthTimeMs =:= (Accessor.Length :: Accessor.Time :: ANil)]
  implicitly[ANil#append[ANil] =:= ANil]
  implicitly[ANil#append[(Accessor.Length :: ANil)] =:= (Accessor.Length :: ANil)]
  implicitly[(Accessor.Length :: ANil)#append[ANil] =:= (Accessor.Length :: ANil)]

  // Test dimlessQuant:
  implicitly[ANil#dimlessQuant =:= Quant]
  implicitly[LengthTimeMs#dimlessQuant =:= Accessor.Length#set[Accessor.Time#set[Quant,_0],_0]]

  // Test setters/getters:
  implicitly[Accessor.Length#get[Accessor.Length#set[Quant,p1]] =:= p1]
  implicitly[Accessor.Time#get[Accessor.Time#set[Speed,n2]] =:= n2]
  implicitly[Accessor.Time#get[Accessor.Time#set[Accessor.Time#set[Quant,n2],n1]] =:= n1]
  implicitly[Accessor.Time#get[Accessor.Time#set[Accessor.Time#set[Quant,n1],n2]] =:= n2]
  implicitly[Accessor.Time#get[TimeMs#dimlessQuant with Accessor.Time#set[Accessor.Time#set[Quant,n1],n2]] =:= n2]

  // Test quantity-level mults:
  implicitly[ANil#op[Quant,Quant]#apply[+] =:= Dimless]
  implicitly[TimeMs#op[Time#quant,Frequency#quant]#apply[+] =:= Dimless]
  implicitly[Speed#accessors#op[Speed#quant, Speed#accessors#dimlessQuant with Time#quant]#apply[+] =:= DimsConst[LengthMs,Length#quant]]

  // Test negations:
  implicitly[Dimless#neg =:= Dimless]
  implicitly[Time#neg =:= Frequency]
  implicitly[Speed#neg#neg =:= Speed]
  implicitly[Speed#neg =:= DimsConst[Accessor.Length :: Accessor.Time :: ANil, Accessor.Time#set[Accessor.Length#set[Quant, n1], p1]]]

  // Test mults and divs
  implicitly[Dimless * Dimless =:= Dimless]
  implicitly[Dimless / Dimless =:= Dimless]

  implicitly[Time * Frequency =:= Dimless]    

  implicitly[Volume / Area =:= Length]
  implicitly[Length * Length =:= Area]

  implicitly[Dimless / Time =:= Frequency]
  implicitly[Length / Time =:= Speed]    
  implicitly[Speed * Time =:= Length]

  // implicitly[Frequency * Length =:= Speed]
  implicitly[Length * Frequency =:= Speed]
}