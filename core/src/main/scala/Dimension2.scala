package scunits

import scunits.integer._
import scunits.integer.Ops._

object Test2 {
  trait Quant
  trait Multer {
    type of <: Quant
    type set[L <: Quant, To <: Integer] <: L with of
    type get[L <: of] <: Integer

    class op[L <: of, R <: of, E <: Quant] {
      type getL = get[L]
      type getR = get[R]
      type apply[O[_ <: Integer, _ <: Integer] <: Integer] = set[E, O[getL,getR]]
    }
  }
  trait MulterOf[S <: Quant] extends Multer {
    type of = S
  }

  trait MList {
    type self <: MList
    type set <: Multer
    type quant <: Quant

    protected type bugMult[L <: quant, R <: quant, E <: Quant] <: Quant
    protected type bugDiv[L <: quant, R <: quant, E <: Quant] <: Quant

    class op[L <: quant, R <: quant] {
      type mult = bugMult[L,R,Quant]
      type div = bugDiv[L,R,Quant]
    }

    type dimless = DimsConst[self, dimlessQuant]
    type dimlessQuant <: quant
    
    type dimOf[I <: NonNegInt] = DimsConst[self, doQuantOf[Quant,I]]

    type quantOf[I <: Integer] = doQuantOf[Quant,I]
    protected type doQuantOf[Q <: Quant, I <: Integer] <: Quant
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
    type dimlessQuant = head#set[Quant,_0] with tail#dimlessQuant

    protected type bugMult[L <: quant, R <: quant, E <: Quant] = tail#bugMult[L,R,head#op[L,R,E]#apply[+]]
    protected type bugDiv[L <: quant, R <: quant, E <: Quant] = tail#bugDiv[L,R,head#op[L,R,E]#apply[-]]
    
    protected type doQuantOf[Q <: Quant, I <: Integer] =
      I#isZero#branch[Quant, head#set[Q,p1] with tail#dimlessQuant, tail#doQuantOf[head#set[Q,_0], I#Pred]]
  }
  trait MNil extends MList {
    type self = MNil
    type set = Multer
    type quant = Quant
    protected type doQuantOf[Q <: Quant, I <: Integer] = Q
    type dimlessQuant = Quant

    protected type bugMult[L <: quant, R <: quant, E <: Quant] = E
    protected type bugDiv[L <: quant, R <: quant, E <: Quant] = E
  }

  trait Dims {
    type multers <: MList
    type quant <: multers#quant

    type mult[R <: DimsOf[multers]] = DimsConst[multers, multers#op[quant,R#quant]#mult]
    type div[R <: DimsOf[multers]] = DimsConst[multers, multers#op[quant,R#quant]#div]
  }
  trait DimsOf[M <: MList] extends Dims {
    type multers = M
  }
  trait DimsConst[M <: MList, Q <: Quant] extends DimsOf[M] {
    // type quant = Q
  }

  object BaseQuantities {
    trait Length extends Quant { type length <: Integer }
    trait Time extends Quant { type time <: Integer }
  }
  trait LengthMulter extends MulterOf[BaseQuantities.Length] {
    type set[L <: Quant, To <: Integer] = L with BaseQuantities.Length { type length = To }
    type get[L <: BaseQuantities.Length] = L#length
  }

  trait TimeMulter extends MulterOf[BaseQuantities.Time] {
    type set[L <: Quant, To <: Integer] = L with BaseQuantities.Time { type time = To }
    type get[L <: BaseQuantities.Time] = L#time
  }

  object Test {
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

    implicitly[LengthQ =:= multersRev#quantOf[p1]]
    implicitly[TimeQ =:= multersRev#quantOf[_0]]

    type Test = multers#op[LengthQ,TimeQ]#mult
    implicitly[multers#op[DimlessQ,DimlessQ]#mult =:= DimlessQ]
    implicitly[multers#op[DimlessQ,DimlessQ]#div =:= DimlessQ]
    implicitly[multers#op[LengthQ,DimlessQ]#mult =:= LengthQ]
    implicitly[multers#op[LengthQ,TimeQ]#mult =:= multers#op[TimeQ,LengthQ]#mult]
    implicitly[multersRev#op[LengthQ,TimeQ]#mult =:= multers#op[LengthQ,TimeQ]#mult]
    implicitly[multers#op[LengthQ,LengthQ]#div =:= DimlessQ]
    implicitly[multers#op[AccelQ,AccelQ]#div =:= DimlessQ]

    // implicitly[Length =:= multersRev#dimOf[p1]]
    // implicitly[Time =:= multersRev#dimOf[_0]]
    // implicitly[Dimless#mult[Dimless] =:= Dimless]
    // val a: Dimless = 1

    // DimsConst[(some other)_74.type(in trait MList),Quant with BaseQuantities.Length{type length = _99.quant#length#Add[DimsConst[_74.type(in trait MList),Quant with BaseQuantities.Length{type length = scunits.integer._0} with BaseQuantities.Time{type time = scunits.integer._0}]#quant#length]} with BaseQuantities.Time{type time = _99.quant#time#Add[DimsConst[_74.type(in trait MList),Quant with BaseQuantities.Length{type length = scunits.integer._0} with BaseQuantities.Time{type time = scunits.integer._0}]#quant#time]}] forSome { val _99: DimsConst[_ <: :*:[LengthMulter,:*:[TimeMulter,MNil]], Quant with BaseQuantities.Length{type length = scunits.integer._0} with BaseQuantities.Time{type time = scunits.integer._0}] }


    type Speed = Length#div[Time]
    type Accel = Speed#div[Time]
  }
}