package scunits

import scunits.nums._

package object tlist {
  trait TList[B] {
    type ::[H <: B, T <: TEl[B]] = TNel[B,H,T]
  }
  sealed trait TEl[B] {
    type Base = B    
    type Map[M <: TMap[B]] <: TEl[M#Out]
    type Tail <: TEl[B]
    // type Zip[R <: TEl[RB],RB] <: TEl[(B,RB)]
    // type MapWith[M <: TMap[B], R <: TEl[B]] <: TEl[M#Out]

    type Zip[R <: TEl[RB],RB] <: TEl[(B,RB)]
    type Zip2[L <: TNel[LB,LH,LT],LB,LH <: LB,LT <: TEl[LB]] <: TEl[(LB,B)]
    // type Zip2[R <: TEl[B],RB] <: TEl[(RB, B)]
  }
  trait TNil[B] extends TEl[B] {
    type Map[M <: TMap[B]] = TNil[M#Out]
    type Tail = TNil[B]
    // type MapWith[M <: TMap[B], R <: TEl[B]] = TNil[M#Out]
    type Zip[R <: TEl[RB],RB] = TNil[(B,RB)]
    type Zip2[L <: TNel[LB,LH,LT],LB,LH <: LB,LT <: TEl[LB]] = TNil[(LB,B)]
  }
  trait TNel[B, H <: B, T <: TEl[B]] extends TEl[B] {
    type Head = H
    type Tail = T
    type Map[M <: TMap[B]] = TNel[M#Out, M#Apply[H], T#Map[M]]
    // type MapWith[M <: TMap[B]] = TNel[M#Out, M#Apply[]]
    type Zip[R <: TEl[RB],RB] = R#Zip2[this.type,B,H,T]
    type Zip2[L <: TNel[LB,LH,LT],LB,LH <: LB,LT <: TEl[LB]] = TNel[(LB,B),(LH,H),LT#Zip[T,B]]
    // type Zip[R <: TNel[RB,_,_],RB] = TNel[(B,RB), (Head, R#Head), T#Zip[R#Tail,RB]]
  }
  trait TMap[B] {
    type Out
    type Apply[I <: B] <: Out
  }

  trait Tup[A,B] {
    type L = A
    type R = B
  }

  trait Adder extends TMap[Tup[Integer,Integer]] {
    type Out = Integer
    type Apply[LR <: Tup[_ <: Integer,_ <: Integer]] = LR#L + LR#R
  }

  object Tests {
    object Ints extends TList[PosInt] {
      type Evens = _0 :: _2 :: _4 :: _6 :: _8 :: TNil[PosInt]
      type Odds = _1 :: _3 :: _5 :: _7 :: _9 :: TNil[PosInt]      
    }
    object Zipped extends TList[(PosInt,PosInt)] {
      type Zipped = (_0,_1) :: (_2,_3) :: (_4,_5) :: (_6,_7) :: (_8,_9) :: TNil[(PosInt,PosInt)]
      type ZipShort = (_2,_1) :: (_4,_3) :: (_6,_5) :: (_8,_7) :: TNil[(PosInt,PosInt)]
    }
    object Strings extends TList[String] {
      type FiveStrings = String :: String :: String :: String :: String :: TNil[String]
    }
    trait ToString extends TMap[PosInt] {
      type Out = String
      type Apply[I <: PosInt] = String
    }
    import Ints._
    import Strings._
    import Zipped._
    implicitly[FiveStrings =:= Evens#Map[ToString]]
    implicitly[Evens#Zip[Odds, PosInt] =:= Zipped]
    implicitly[Evens#Tail#Zip[Odds, PosInt] =:= ZipShort]
  }
}