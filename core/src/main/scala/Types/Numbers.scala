package scunits.types

import scunits._

trait Integer extends ComparableWith[Integer] {
  type This <: Integer
  type IsZero <: Bool
  type IsPos <: Bool
  type IsNeg <: Bool
  type Succ <: Integer
  
  type Add[N <: Integer] <: Integer
  type AddNN[N <: NonNegInt] <: Integer
  type AddNP[N <: NonPosInt] <: Integer

  type Pred <: Integer
  type Sub[N <: Integer] <: Integer
  type Neg <: Integer

  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] <: B
  type DimMag[B <: BaseQuantityLike, T <: Dims] <: Dims

  type Compare[R <: Integer] = (This#Sub[R])#Comp
  type Comp <: Compared
}

trait NonNegInt extends Integer {
  type IsNeg = False
  type Succ <: PosInt
  type AddNN[N <: NonNegInt] <: NonNegInt
}
trait NonPosInt extends Integer {
  type IsPos = False
  type Pred <: NegInt
  type AddNP[N <: NonPosInt] <: NonPosInt
}
trait NonZeroInt extends Integer {
  type IsZero = False
  type DimMag[B <: BaseQuantityLike, T <: Dims] = DNelConst[B,This,T]
}
trait NegInt extends NonPosInt with NonZeroInt {
  type IsNeg = True
  type Succ <: NonPosInt
  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = N
  type Comp = Less
  type AddNP[N <: NonPosInt] <: NegInt
}
trait PosInt extends NonNegInt with NonZeroInt {
  type IsPos = True
  type Pred <: NonNegInt
  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = P
  type Comp = Greater
  type AddNN[N <: NonNegInt] <: PosInt
}

class SuccInt[P <: NonNegInt] extends PosInt {
  type This = SuccInt[P]
  type Succ = SuccInt[SuccInt[P]]
  type Add[N <: Integer] = P#Add[N]#Succ
  type AddNN[N <: NonNegInt] = P#AddNN[N]#Succ
  type AddNP[N <: NonPosInt] = Add[N]
  type Pred = P
  type Sub[N <: Integer] = P#Sub[N]#Succ
  type Neg = P#Neg#Pred
}

class PredInt[S <: NonPosInt] extends NegInt {
  type This = PredInt[S]
  type Succ = S
  type Add[N <: Integer] = S#Add[N]#Pred
  type AddNN[N <: NonNegInt] = Add[N]
  type AddNP[N <: NonPosInt] = S#AddNP[N]#Pred 
  type Pred = PredInt[PredInt[S]]
  type Sub[N <: Integer] = S#Sub[N]#Pred
  type Neg = S#Neg#Succ
}

final class i0 extends NonNegInt with NonPosInt {
  type This = i0
  type IsZero = True
  type Succ = SuccInt[i0]
  type Add[N <: Integer] = N
  type AddNN[N <: NonNegInt] = N
  type AddNP[N <: NonPosInt] = N
  type Pred = PredInt[i0]
  type Sub[N <: Integer] = N#Neg
  type Neg = i0
  type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = Z
  type Comp = Equal

  type DimMag[B <: BaseQuantityLike, T <: Dims] = T
}