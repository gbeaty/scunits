package scunits

package object nums {

  trait Integer {
    type Succ <: Integer
    type Add[N <: Integer] <: Integer
    type Pred <: Integer
    type Sub[N <: Integer] <: Integer
    type Neg <: Integer
  }

  trait PosInt extends Integer
  trait NegInt extends Integer
  type Count = SuccInt[_]

  class SuccInt[P <: PosInt] extends PosInt {
    type Succ = SuccInt[SuccInt[P]]
    type Add[N <: Integer] = P#Add[N]#Succ
    type Pred = P
    type Sub[N <: Integer] = P#Sub[N]#Succ
    type Neg = P#Neg#Pred
  }

  class PredInt[S <: NegInt] extends NegInt {
    type Succ = S
    type Add[N <: Integer] = S#Add[N]#Pred
    type Pred = PredInt[PredInt[S]]
    type Sub[N <: Integer] = S#Sub[N]#Pred
    type Neg = S#Neg#Succ
  }

  final class _0 extends PosInt with NegInt {
    type Succ = SuccInt[_0]
    type Add[N <: Integer] = N
    type Pred = PredInt[_0]
    type Sub[N <: Integer] = N#Neg
    type Neg = _0
  }

  type +[N1<:Integer, N2<:Integer] = N1#Add[N2]
  type -[N1<:Integer, N2<:Integer] = N1#Sub[N2]

  type _1 = _0#Succ
  type _2 = _1#Succ
  type _3 = _2#Succ
  type _4 = _3#Succ
  type _5 = _4#Succ
  type _6 = _5#Succ
  type _7 = _6#Succ
  type _8 = _7#Succ
  type _9 = _8#Succ

  implicitly[_0 + _0 =:= _0]
  implicitly[_0 - _0 =:= _0]
  implicitly[_1 + _2 =:= _3]
  implicitly[_1 - _2 =:= _1#Neg]
  implicitly[_1#Neg + _2 =:= _1]
  implicitly[_1#Neg + _2#Neg =:= _3#Neg]
}