package scunits

import scunits.types._

package object integer {

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

  type Count = SuccInt[_]

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

  object Ops {
    type +[L <: Integer, R <: Integer] = L#Add[R]
    type -[L <: Integer, R <: Integer] = L#Sub[R]
  }

  type p1 = _0#Succ
  type p2 = p1#Succ
  type p3 = p2#Succ
  type p4 = p3#Succ
  type p5 = p4#Succ
  type p6 = p5#Succ
  type p7 = p6#Succ
  type p8 = p7#Succ
  type p9 = p8#Succ

  type n1 = _0#Pred
  type n2 = n1#Pred
  type n3 = n2#Pred
  type n4 = n3#Pred
  type n5 = n4#Pred
  type n6 = n5#Pred
  type n7 = n6#Pred
  type n8 = n7#Pred
  type n9 = n8#Pred
}