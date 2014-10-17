package scunits.types

object BQ {
  trait Length extends Quant { type length <: Integer }
  trait Time extends Quant { type time <: Integer }
  trait Info extends Quant { type info <: Integer }
}
object Accessors {
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

object Dims2Tests {
  type ^[L <: Accessor, R <: Integer] = L#set[Quant,R]
  type LengthMs = Accessors.Length :: ANil
  type TimeMs = Accessors.Time :: ANil
  type InfoMs = Accessors.Info :: ANil

  type Length = Accessors.Length#to[p1]
  type Area = Accessors.Length#to[p2]
  type Volume = Accessors.Length#to[p3]
  type Time = Accessors.Time#to[p1]
  type Info = Accessors.Info#to[p1]
  type Frequency = Dims2Const[Accessors.Time :: ANil, Accessors.Time#set[Quant, n1]]
  type Speed = Dims2Const[Accessors.Length :: Accessors.Time :: ANil, Accessors.Time#set[Accessors.Length#set[Quant, p1], n1]]
  type Accel = Dims2Const[Accessors.Length :: Accessors.Time :: ANil, Accessors.Time#set[Accessors.Length#set[Quant, p1], n2]]
  type Bandwidth = Dims2Const[Accessors.Info :: Accessors.Time :: ANil, Accessors.Time#set[Accessors.Info#set[Quant, p1], n1]]

  // Test base:
  implicitly[Time =:= Dims2Const[Accessors.Time :: ANil, Quant with BQ.Time { type time = p1 }]]

  // Test append:
  type LengthTimeMs = LengthMs#append[TimeMs]
  implicitly[LengthTimeMs =:= (Accessors.Length :: Accessors.Time :: ANil)]
  implicitly[ANil#append[ANil] =:= ANil]
  implicitly[ANil#append[(Accessors.Length :: ANil)] =:= (Accessors.Length :: ANil)]
  implicitly[(Accessors.Length :: ANil)#append[ANil] =:= (Accessors.Length :: ANil)]

  // Test dimlessQuant:
  implicitly[ANil#dimlessQuant =:= Quant]
  implicitly[LengthTimeMs#dimlessQuant =:= Accessors.Length#set[Accessors.Time#set[Quant,_0],_0]]

  // Test setters/getters:
  implicitly[Accessors.Length#get[Accessors.Length#set[Quant,p1]] =:= p1]
  implicitly[Accessors.Time#get[Accessors.Time#set[Speed,n2]] =:= n2]
  implicitly[Accessors.Time#get[Accessors.Time#set[Accessors.Time#set[Quant,n2],n1]] =:= n1]
  implicitly[Accessors.Time#get[Accessors.Time#set[Accessors.Time#set[Quant,n1],n2]] =:= n2]
  implicitly[Accessors.Time#get[TimeMs#dimlessQuant with Accessors.Time#set[Accessors.Time#set[Quant,n1],n2]] =:= n2]

  // Test quantity-level mults:
  implicitly[ANil#op[Quant,Quant]#apply[+] =:= Dimless]
  implicitly[TimeMs#op[Time#quant,Frequency#quant]#apply[+] =:= Dimless]
  implicitly[Speed#accessors#op[Speed#quant, Speed#accessors#dimlessQuant with Time#quant]#apply[+] =:= Dims2Const[LengthMs,Length#quant]]

  // Test negations:
  implicitly[Dimless#neg =:= Dimless]
  implicitly[Time#neg =:= Frequency]
  implicitly[Speed#neg#neg =:= Speed]
  implicitly[Speed#neg =:= Dims2Const[Accessors.Length :: Accessors.Time :: ANil, Accessors.Time#set[Accessors.Length#set[Quant, n1], p1]]]

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