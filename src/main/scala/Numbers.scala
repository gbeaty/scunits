package scunits

import scunits.bool._

package object integer {

  trait Comparable[S] {
    type Eq[R <: S] <: Bool
  }

  trait Integer extends Comparable[Integer] {
    type Succ <: Integer
    type Add[N <: Integer] <: Integer
    type Pred <: Integer
    type Sub[N <: Integer] <: Integer    
    type Neg <: Integer

    type IsZero <: Bool
    type IsPos <: Bool
    type IsNeg <: Bool

    type Eq[R <: Integer] = Sub[R]#IsZero
    type Greater[R <: Integer] = Sub[R]#IsPos
    type Less[R <: Integer] = Sub[R]#IsNeg
    type GreaterOrEq[R <: Integer] = Sub[R]#IsNeg#Not
    type LessOrEq[R <: Integer] = Sub[R]#IsPos#Not
  }

  trait NonNegInt extends Integer {
    type IsNeg = False
  }
  trait NonPosInt extends Integer {
    type IsPos = False
  }
  trait NonZeroInt extends Integer {
    type IsZero = False
  }
  trait NegInt extends NonPosInt with NonZeroInt {
    type IsNeg = True
  }
  trait PosInt extends NonNegInt with NonZeroInt{
    type IsPos = True
  }  
  type Count = SuccInt[_]

  class SuccInt[P <: NonNegInt] extends PosInt {
    type Succ = SuccInt[SuccInt[P]]
    type Add[N <: Integer] = P#Add[N]#Succ
    type Pred = P
    type Sub[N <: Integer] = P#Sub[N]#Succ
    type Neg = P#Neg#Pred
  }

  class PredInt[S <: NonPosInt] extends NegInt {
    type Succ = S
    type Add[N <: Integer] = S#Add[N]#Pred
    type Pred = PredInt[PredInt[S]]
    type Sub[N <: Integer] = S#Sub[N]#Pred
    type Neg = S#Neg#Succ
  }

  final class _0 extends NonNegInt with NonPosInt {
    type Succ = SuccInt[_0]
    type Add[N <: Integer] = N
    type Pred = PredInt[_0]
    type Sub[N <: Integer] = N#Neg
    type Neg = _0
  }

  object Ops {
    type ==[L <: Integer, R <: Integer] = L#Eq[R]
    type >[L <: Integer, R <: Integer] = L#Greater[R]
    type <[L <: Integer, R <: Integer] = L#Less[R]
    type >=[L <: Integer, R <: Integer] = L#GreaterOrEq[R]
    type <=[L <: Integer, R <: Integer] = L#LessOrEq[R]
    type +[L <: Integer, R <: Integer] = L#Add[R]
    type -[L <: Integer, R <: Integer] = L#Sub[R]
  }

  type _1 = _0#Succ
  type _2 = _1#Succ
  type _3 = _2#Succ
  type _4 = _3#Succ
  type _5 = _4#Succ
  type _6 = _5#Succ
  type _7 = _6#Succ
  type _8 = _7#Succ
  type _9 = _8#Succ

  import Ops._

  implicitly[_0 + _0 =:= _0]
  implicitly[_0 - _0 =:= _0]
  implicitly[_1 + _2 =:= _3]
  implicitly[_1 - _2 =:= _1#Neg]
  implicitly[_1#Neg + _2 =:= _1]
  implicitly[_1#Neg + _2#Neg =:= _3#Neg]

  implicitly[_0 > _0 =:= False]
  implicitly[_3 > _0 =:= True]
  implicitly[_0 > _3 =:= False]
  implicitly[_3#Neg > _0 =:= False]
  implicitly[_3 > _2#Neg =:= True]

  implicitly[_0 < _0 =:= False]
  implicitly[_3 < _0 =:= False]
  implicitly[_0 < _3 =:= True]
  implicitly[_3#Neg < _0 =:= True]
  implicitly[_3 < _2#Neg =:= False]

  implicitly[_0 >= _0 =:= True]
  implicitly[_3 >= _0 =:= True]
  implicitly[_0 >= _3 =:= False]
  implicitly[_3#Neg >= _0 =:= False]
  implicitly[_3 >= _2#Neg =:= True]

  implicitly[_0 <= _0 =:= True]
  implicitly[_3 <= _0 =:= False]
  implicitly[_0 <= _3 =:= True]
  implicitly[_3#Neg <= _0 =:= True]
  implicitly[_3 <= _2#Neg =:= False]
}