package scunits

import scunits.integer._
import scunits.bool._

package object tlist2 {
  trait TList
  trait TNil extends TList
  trait TNel[H,T <: TList] extends TList {
    type Head = H
    type Tail = T
  }
  trait TOp[L <: TList, End <: TNil, Nel[_,_ <: TList]]
}

package object tlist {

  trait TList[B] {
    type ::[H <: B, T <: TEl[B]] = TNel[B,H,T]
  }
  trait TEl[B] {
    type Base = B
    type Empty <: Bool
    type Map[M <: TMap[B]] <: TEl[M#Out]
    type Tail <: TEl[B]
    type Zip[R <: TEl[RB],RB] <: TEl[Tup2[B,RB]]
    type Zip2[L <: TNel[LB,LH,LT],LB,LH <: LB,LT <: TEl[LB]] <: TEl[Tup2[LB,B]]
  }
  trait TNil[B] extends TEl[B] {
    type Empty = True
    type Map[M <: TMap[B]] = TNil[M#Out]
    type Tail = TNil[B]
    type Zip[R <: TEl[RB],RB] = TNil[Tup2[B,RB]]
    type Zip2[L <: TNel[LB,LH,LT],LB,LH <: LB,LT <: TEl[LB]] = TNil[Tup2[LB,B]]
  }
  trait TNel[B, H <: B, T <: TEl[B]] extends TEl[B] {
    type Empty = False
    type Head = H
    type Tail = T
    type Map[M <: TMap[B]] = TNel[M#Out, M#Apply[H], T#Map[M]]
    type Zip[R <: TEl[RB],RB] = R#Zip2[this.type,B,H,T]
    type Zip2[L <: TNel[LB,LH,LT],LB,LH <: LB,LT <: TEl[LB]] = TNel[Tup2[LB,B],Tup2[LH,H],LT#Zip[T,B]]
  }
  trait TMap[B] {
    type Out
    type Apply[I <: B] <: Out
  }

  trait Tup2[+A,+B] {
    type L <: A
    type R <: B
  }

  trait Op[O[_ <: Integer,_ <: Integer] <: Integer] extends TMap[Tup2[Integer,Integer]] {
    type Out = Integer
    type Apply[LR <: Tup2[_ <: Integer,_ <: Integer]] = O[LR#L,LR#R]
  }

  object Tests {
    object Ints extends TList[Integer] {
      type Evens = _0 :: _2 :: TNil[Integer]
      type Odds =  _1 :: _3 :: TNil[Integer]
      type EvensMinusOdds = _1#Neg :: _1#Neg :: TNil[Integer]
      type OddsMinusEvens = _1     :: _1     :: TNil[Integer]
    }
    object Zipped extends TList[Tup2[Integer,Integer]] {
      type Zipped =   Tup2[_0,_1] :: Tup2[_2,_3] :: TNil[Tup2[Integer,Integer]]
      type ZipShort = Tup2[_2,_1]               :: TNil[Tup2[Integer,Integer]]
    }
    object Strings extends TList[String] {
      type FiveStrings = String :: String :: TNil[String]
    }
    trait ToString extends TMap[Integer] {
      type Out = String
      type Apply[I <: Integer] = String
    }
    import Ints._
    import Strings._
    import Zipped._
    implicitly[FiveStrings =:= Evens#Map[ToString]]
    type EvensAndOdds = Evens#Zip[Odds, Integer]
    type OddsAndEvens = Odds#Zip[Evens, Integer]
    implicitly[EvensAndOdds =:= Zipped]
    implicitly[Evens#Tail#Zip[Odds, Integer] =:= ZipShort]
    
    // val t: OddsAndEvens#Map[Op[-]] = 1
    // TNel[Integer,P#Sub[_0]#Succ,TNel[Integer,P#Sub[_2]#Succ,TNil[Integer]]]
    // TNel[Integer, _1, TNel[Integer, _1, TNil[Integer]]]


    //implicitly[OddsAndEvens#Map[Op[-]] =:= OddsMinusEvens]
    //- val a: OddsAndEvens#Map[Op[-]] = 1
    // TNel[Op[-]#Out,Op[-]#Apply[Tup[SuccInt[_0],_0]],TNel[Op[-]#Out,Op[-]#Apply[Tup[SuccInt[SuccInt[SuccInt[_0]]],SuccInt[SuccInt[_0]]]],TNil[Op[-]#Out]]]
    // TNel[Op[-]#Out,Op[-]#Apply[Tup[_1,_0]],TNel[Op[-]#Out,Op[-]#Apply[Tup[_3,_2]],TNil[Op[-]#Out]]]
    // TNel[Integer,Op[-]#Apply[Tup[_1,_0]],TNel[Integer,Op[-]#Apply[Tup[_3,_2]],TNil[Integer]]]
    // TNel[Integer, _1, TNel[Integer, _1, TNil[Integer]]]

    //- val b: OddsMinusEvens = 2
    // TNel[Integer, _1, TNel[Integer, _1, TNil[Integer]]]
  }
}