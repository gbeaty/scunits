package scunits

import scunits.integer._
import scunits.integer.Ops._

object Test2 {
  trait Quant
  trait Dim {
    type multer <: Multer
    type exp <: Integer    
  }
  trait ^[L <: Multer, R <: Integer] extends Dim {
    type multer = L
    type exp = R
  }
  trait Multer {
    type self <: Multer
    type of <: Quant
    type set[L <: Quant, To <: Integer] = setDim[L, self ^ To]
    type get[L <: of] = getDim[L]#exp
    // type base = DimsConst[self :*: MNil, set[Quant,p1]]

    type setDim[L <: Quant, To <: Dim] <: L with of
    type getDim[L <: of] <: Dim

    class op[L <: of, R <: of, E <: Quant] {
      type getL = get[L]
      type getR = get[R]
      type apply[O[_ <: Integer, _ <: Integer] <: Integer] = set[E, O[getL,getR]]
    }
  }
  trait MulterOf[S <: MulterOf[S,Q], Q <: Quant] extends Multer {
    type self = S
    type of = Q    
  }

  trait MList {
    type self <: MList
    type set <: Multer
    type quant <: Quant    

    protected type bugMult[L <: quant, R <: quant] <: quant
    protected type bugDiv[L <: quant, R <: quant] <: quant

    protected type bugMult2[L <: quant, R <: quant] <: quant
    
    type quantOf[I <: Integer] <: quant
    type dimlessQuant <: quant
    type append[Ms <: MList] <: MList

    type buildDims[F <: quant] <: Dims

    class op[L <: quant, R <: quant] {
      type mult = bugMult[L,R]
      type div = bugDiv[L,R]
    }

    class op2[L <: quant, R <: quant] {
      type mult <: quant
    }

    type dimless = DimsConst[self, self#dimlessQuant]
    type dimOf[I <: NonNegInt] = DimsConst[self, self#quantOf[I]]
  }
  trait MNel extends MList {
    type head <: Multer
    type tail <: MList
  }
  trait :*:[L <: Multer, R <: MList] extends MNel {
    type head = L
    type tail = R
    type self = head :*: tail
    type set = head with tail#set
    type quant = head#of with tail#quant
    type dimlessQuant = head#set[tail#dimlessQuant, _0]
    type append[Ms <: MList] = head :*: tail#append[Ms]

    /*type buildDims[F <: quant] = ({
      type dim = head#getDim[F]
      type res = dim#exp#isZero#branch[
        Dims,
        tail#buildDims[F],
        ({
          type rem = tail#buildDims[F]
          type all = DimsConst[dim#multer :*: rem#multers, rem#quant with dim]
        })#all
      ]
    })#res*/

    protected type bugMult[L <: quant, R <: quant] = head#op[L,R,tail#bugMult[L,R]]#apply[+]
    protected type bugDiv[L <: quant, R <: quant] = head#op[L,R,tail#bugDiv[L,R]]#apply[-]

    type quantOf[I <: Integer] = I#isZero#branch[
      quant,
      head#set[tail#dimlessQuant, p1],
      head#set[tail#quantOf[I#Pred], _0]
    ]
  }
  trait MNil extends MList {
    type self = MNil
    type set = Multer
    type quant = Quant
    type quantOf[I <: Integer] = Quant
    type dimlessQuant = Quant
    type append[Ms <: MList] = Ms

    type buildDims[F <: quant] = Dimless2

    protected type bugMult[L <: quant, R <: quant] = Quant
    protected type bugDiv[L <: quant, R <: quant] = Quant
  }

  trait Dims {
    type multers <: MList
    type quant <: multers#quant

    class op[R <: DimsOf[multers]] {
      type mult = DimsConst[multers, multers#op[quant,R#quant]#mult]
      type div = DimsConst[multers, multers#op[quant,R#quant]#div]
    }

    type mult2[R <: Dims] = ({
      type ms = multers#append[R#multers]
      type dless = ms#dimlessQuant
      type res = ms#op[dless with quant, dless with R#quant]#mult
    })#res

    type mult[R <: DimsOf[multers]] = DimsConst[multers, multers#op[quant,R#quant]#mult]
    type div[R <: DimsOf[multers]] = DimsConst[multers, multers#op[quant,R#quant]#div]

    type merge[R <: Dims] = ({
      type ms = multers#append[R#multers]
      type res = DimsConst[ms, ms#dimlessQuant with R#quant]
    })#res
  }
  trait DimsOf[M <: MList] extends Dims {
    type multers = M
  }
  trait DimsConst[M <: MList, Q <: M#quant] extends DimsOf[M] {
    type quant = Q
  }
  trait Dimless2 extends DimsConst[MNil,Quant]

  object BQs {
    trait Length extends Quant { type length <: Dim }
    trait Time extends Quant { type time <: Dim }
  }
  trait LengthMulter extends MulterOf[LengthMulter, BQs.Length] {
    type setDim[L <: Quant, To <: Dim] = L with BQs.Length { type length = To }
    type getDim[L <: of] = L#length
  }

  trait TimeMulter extends MulterOf[TimeMulter, BQs.Time] {
    type setDim[L <: Quant, To <: Dim] = L with BQs.Time { type time = To }
    type getDim[L <: of] = L#time
  }

  object Test {
    type lenMulters = LengthMulter :*: MNil    
    type multers = LengthMulter :*: TimeMulter :*: MNil
    type multersRev = TimeMulter :*: LengthMulter :*: MNil
    type Dimless = multers#dimless
    type Length = multers#dimOf[_0]
    type Time = multers#dimOf[p1]

    implicitly[multers#quantOf[_0] =:= multersRev#quantOf[p1]]

    type DimlessQ = multers#dimlessQuant
    type LengthQ = multers#quantOf[_0]
    type TimeQ = multers#quantOf[p1]
    type SpeedQ = multers#op[LengthQ,TimeQ]#div
    type AccelQ = multers#op[SpeedQ,TimeQ]#div
    type Frequency = multers#op[DimlessQ,TimeQ]#div

    // Test quantOf:
    implicitly[LengthQ =:= multersRev#quantOf[p1]]
    implicitly[TimeQ =:= multersRev#quantOf[_0]]

    // Test quant mult/div:
    type Test = multers#op[LengthQ,TimeQ]#mult
    implicitly[multers#op[DimlessQ,DimlessQ]#mult =:= DimlessQ]
    implicitly[multers#op[DimlessQ,DimlessQ]#div =:= DimlessQ]
    implicitly[multers#op[LengthQ,DimlessQ]#mult =:= LengthQ]
    implicitly[multers#op[LengthQ,TimeQ]#mult =:= multers#op[TimeQ,LengthQ]#mult]
    implicitly[multersRev#op[LengthQ,TimeQ]#mult =:= multers#op[LengthQ,TimeQ]#mult]
    implicitly[multers#op[LengthQ,LengthQ]#div =:= DimlessQ]
    implicitly[multers#op[AccelQ,AccelQ]#div =:= DimlessQ]

    type Speed = Length#op[Time]#div
    type Accel = Speed#op[Time]#div

    implicitly[Accel#op[Accel]#div =:= Dimless]
    implicitly[Dimless#op[Dimless]#div =:= Dimless]
    implicitly[Dimless#op[Dimless]#mult =:= Dimless]

    // Test merge:
    // implicitly[lenMulters]
  }
}

/*
  AB * BC = AB2C
  mults = ABBC
  BC#dimless with AB
  AB#dimless with BC
*/
