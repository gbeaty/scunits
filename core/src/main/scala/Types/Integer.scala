package scunits.types

import scunits._

trait Integer {
  type Self <: Integer
  type Succ <: Integer
  type Add[N <: Integer] <: Integer
  type Pred <: Integer
  type Sub[N <: Integer] <: Integer    
  type Neg <: Integer
  type isZero <: Bool
  type isPos <: Bool
  type isNeg <: Bool

  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] <: B

  type DimMag[B <: BaseQuantityLike, T <: Dims] <: Dims
}

trait NonNegInt extends Integer {
  type isNeg = False
}
trait NonPosInt extends Integer {
  type isPos = False
}
trait NonZeroInt extends Integer {    
  type DimMag[B <: BaseQuantityLike, T <: Dims] = DNelConst[B,Self,T]
  type isZero = False
}
trait NegInt extends NonPosInt with NonZeroInt {
  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = N
  type isNeg = True
}
trait PosInt extends NonNegInt with NonZeroInt{
  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = P
  type isPos = True
}

class SuccInt[P <: NonNegInt] extends PosInt {
  type Self = SuccInt[P]
  type Succ = SuccInt[SuccInt[P]]
  type Add[N <: Integer] = P#Add[N]#Succ
  type Pred = P
  type Sub[N <: Integer] = P#Sub[N]#Succ
  type Neg = P#Neg#Pred
}

class PredInt[S <: NonPosInt] extends NegInt {
  type Self = PredInt[S]
  type Succ = S
  type Add[N <: Integer] = S#Add[N]#Pred
  type Pred = PredInt[PredInt[S]]
  type Sub[N <: Integer] = S#Sub[N]#Pred
  type Neg = S#Neg#Succ
}

final class _0 extends NonNegInt with NonPosInt {
  type Self = _0
  type Succ = SuccInt[_0]
  type Add[N <: Integer] = N
  type Pred = PredInt[_0]
  type Sub[N <: Integer] = N#Neg
  type Neg = _0
  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = Z
  type isZero = True

  type DimMag[B <: BaseQuantityLike, T <: Dims] = T
}