package scunits

package object integer {

  trait Integer {
    type Self <: Integer
    type Succ <: Integer
    type Add[N <: Integer] <: Integer
    type Pred <: Integer
    type Sub[N <: Integer] <: Integer    
    type Neg <: Integer

    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] <: B

    type DimMag[B <: BaseQuantityLike, T <: Dimensions] <: Dimensions
  }
  trait IntegerConst[I <: Integer] extends Integer {
    type Self = I
  }

  trait NonNegInt extends Integer
  trait NonNegIntConst[I <: NonNegInt] extends IntegerConst[I] with NonNegInt

  trait NonPosInt extends Integer
  trait NonPosIntConst[I <: NonPosInt] extends IntegerConst[I] with NonPosInt

  trait NonZeroInt extends Integer {
    type DimMag[B <: BaseQuantityLike, T <: Dimensions] = DNel[B,Self,T]
  }
  trait NonZeroIntConst[I <: NonZeroInt] extends IntegerConst[I] with NonZeroInt

  trait NegInt extends NonPosInt with NonZeroInt {
    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = N
  }
  trait NegIntConst[I <: NegInt] extends NonPosIntConst[I] with NonZeroIntConst[I] with NegInt

  trait PosInt extends NonNegInt with NonZeroInt{
    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = P
  }
  trait PosIntConst[I <: PosInt] extends NonNegIntConst[I] with NonZeroIntConst[I] with PosInt

  type Count = SuccInt[_]

  class SuccInt[P <: NonNegInt] extends PosIntConst[SuccInt[P]] {
    type Succ = SuccInt[SuccInt[P]]
    type Add[N <: Integer] = P#Add[N]#Succ
    type Pred = P
    type Sub[N <: Integer] = P#Sub[N]#Succ
    type Neg = P#Neg#Pred
  }

  class PredInt[S <: NonPosInt] extends NegIntConst[PredInt[S]] {
    type Succ = S
    type Add[N <: Integer] = S#Add[N]#Pred
    type Pred = PredInt[PredInt[S]]
    type Sub[N <: Integer] = S#Sub[N]#Pred
    type Neg = S#Neg#Succ
  }

  final class _0 extends NonNegIntConst[_0] with NonPosIntConst[_0] {
    type Succ = SuccInt[_0]
    type Add[N <: Integer] = N
    type Pred = PredInt[_0]
    type Sub[N <: Integer] = N#Neg
    type Neg = _0
    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = Z

    type DimMag[B <: BaseQuantityLike, T <: Dimensions] = T
  }

  object Ops {
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
}