package scunits

import scunits.integer._
import scunits.integer.Ops._

package object dims2 {
  trait Quant

  trait Multer {
    type self <: Multer
    type of
    type set[L <: Quant, To <: Integer] <: L with of
    type get[L <: of] <: Integer
    type to[E <: Integer] <: DimsOf[self :: MNil]

    class op[L <: of, R <: of, E <: Quant] {
      type getL = get[L]
      type getR = get[R]
      type apply[O[_ <: Integer, _ <: Integer] <: Integer] = set[E, O[getL,getR]]
    }
  }
  trait MulterOf[S <: MulterOf[S,Of], Of] extends Multer {
    type self = S
    type of = Of

    type to[E <: Integer] = DimsConst[self :: MNil, set[Quant,E]]
  }

  trait MList {
    type self <: MList
    type set <: Multer
    type quant <: Quant   

    protected type bugNeg[L <: quant] <: quant
    protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] <: Dims
    
    type dimlessQuant <: quant
    type append[Ms <: MList] <: MList

    type dimsOf[Q <: quant] <: DimsOf[self]

    class op[L <: quant, R <: quant] {
      type apply[O[_ <: Integer, _ <: Integer] <: Integer] = bugOp[L,R,O]
    }
    class neg[L <: quant] {
      type apply = bugNeg[L]
    }

    type dimless = DimsConst[self, self#dimlessQuant]
  }
  trait MNel extends MList {
    type head <: Multer
    type tail <: MList
  }
  trait ::[L <: Multer, R <: MList] extends MNel {
    type head = L
    type tail = R
    type self = head :: tail
    type set = head with tail#set
    type quant = head#of with tail#quant
    type dimlessQuant = head#set[tail#dimlessQuant, _0]
    type append[Ms <: MList] = head :: tail#append[Ms]

    protected type bugNeg[L <: quant] = head#set[tail#bugNeg[L], head#get[L]#Neg]

    protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] = ({
      type exp = O[head#get[L], head#get[R]]
      type rem = tail#bugOp[head#set[L,_0], head#set[R,_0], O]
      type res = exp#isZero#branch[Dims, rem, DimsConst[head :: rem#multers, head#set[rem#quant,exp]]]
    })#res
  }

  trait MNil extends MList {
    type self = MNil
    type set = Multer
    type quant = Quant
    type dimlessQuant = Quant
    type append[Ms <: MList] = Ms

    protected type bugNeg[L <: quant] = Quant
    protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] = Dimless
  }

  trait Dims {
    type multers <: MList
    type quant <: multers#quant

    type neg = DimsConst[multers, multers#neg[quant]#apply]
  }
  trait DimsOf[Ms <: MList] extends Dims {
    type multers = Ms
  }
  trait DimsConst[Ms <: MList, Q <: Ms#quant] extends DimsOf[Ms] {
    type quant = Q
  }
  type Dimless = DimsConst[MNil,Quant]

  object BQ {
    trait Length extends Quant { type length <: Integer }
    trait Time extends Quant { type time <: Integer }
    trait Info extends Quant { type info <: Integer }
  }
  object Multer {
    trait Length extends MulterOf[Length, BQ.Length] {
      type set[L <: Quant, To <: Integer] = L with BQ.Length { type length = To }
      type get[L <: of] = L#length
    }
    trait Time extends MulterOf[Time, BQ.Time] {
      type set[L <: Quant, To <: Integer] = L with BQ.Time { type time = To }
      type get[L <: of] = L#time
    }
    trait Info extends MulterOf[Info, BQ.Info] {
      type set[L <: Quant, To <: Integer] = L with BQ.Info { type info = To }
      type get[L <: of] = L#info
    }
  }

  type op[L <: Dims, R <: Dims, O[_ <: Integer, _ <: Integer] <: Integer] = ({
    type ms = L#multers#append[R#multers]
    type base = ms#dimlessQuant
    type res = ms#op[base with L#quant, base with R#quant]#apply[O]
  })#res

  type mult[L <: Dims, R <: Dims] = op[L,R,+]
  type div[L <: Dims, R <: Dims] = op[L,R,-]

  object Ops {
    type *[L <: Dims, R <: Dims] = mult[L,R]
    type /[L <: Dims, R <: Dims] = div[L,R]
  }
  import Ops._

  object Test {
    type ^[L <: Multer, R <: Integer] = L#set[Quant,R]
    type LengthMs = Multer.Length :: MNil
    type TimeMs = Multer.Time :: MNil
    type InfoMs = Multer.Info :: MNil

    type Length = Multer.Length#to[p1]
    type Area = Multer.Length#to[p2]
    type Volume = Multer.Length#to[p3]
    type Time = Multer.Time#to[p1]
    type Info = Multer.Info#to[p1]
    type Frequency = DimsConst[Multer.Time :: MNil, Multer.Time#set[Quant, n1]]
    type Speed = DimsConst[Multer.Length :: Multer.Time :: MNil, Multer.Time#set[Multer.Length#set[Quant, p1], n1]]
    type Accel = DimsConst[Multer.Length :: Multer.Time :: MNil, Multer.Time#set[Multer.Length#set[Quant, p1], n2]]
    type Bandwidth = DimsConst[Multer.Info :: Multer.Time :: MNil, Multer.Time#set[Multer.Info#set[Quant, p1], n1]]

    // Test base:
    implicitly[Time =:= DimsConst[Multer.Time :: MNil, Quant with BQ.Time { type time = p1 }]]

    // Test append:
    type LengthTimeMs = LengthMs#append[TimeMs]
    implicitly[LengthTimeMs =:= (Multer.Length :: Multer.Time :: MNil)]
    implicitly[MNil#append[MNil] =:= MNil]
    implicitly[MNil#append[(Multer.Length :: MNil)] =:= (Multer.Length :: MNil)]
    implicitly[(Multer.Length :: MNil)#append[MNil] =:= (Multer.Length :: MNil)]

    // Test dimlessQuant:
    implicitly[MNil#dimlessQuant =:= Quant]
    implicitly[LengthTimeMs#dimlessQuant =:= Multer.Length#set[Multer.Time#set[Quant,_0],_0]]

    // Test setters/getters:
    implicitly[Multer.Length#get[Multer.Length#set[Quant,p1]] =:= p1]
    implicitly[Multer.Time#get[Multer.Time#set[Speed,n2]] =:= n2]
    implicitly[Multer.Time#get[Multer.Time#set[Multer.Time#set[Quant,n2],n1]] =:= n1]
    implicitly[Multer.Time#get[Multer.Time#set[Multer.Time#set[Quant,n1],n2]] =:= n2]
    implicitly[Multer.Time#get[TimeMs#dimlessQuant with Multer.Time#set[Multer.Time#set[Quant,n1],n2]] =:= n2]

    // Test quantity-level mults:
    implicitly[MNil#op[Quant,Quant]#apply[+] =:= Dimless]
    implicitly[TimeMs#op[Time#quant,Frequency#quant]#apply[+] =:= Dimless]
    implicitly[Speed#multers#op[Speed#quant, Speed#multers#dimlessQuant with Time#quant]#apply[+] =:= DimsConst[LengthMs,Length#quant]]

    // Test negations:
    implicitly[Dimless#neg =:= Dimless]
    implicitly[Time#neg =:= Frequency]
    implicitly[Speed#neg#neg =:= Speed]
    implicitly[Speed#neg =:= DimsConst[Multer.Length :: Multer.Time :: MNil, Multer.Time#set[Multer.Length#set[Quant, n1], p1]]]

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
  }
}