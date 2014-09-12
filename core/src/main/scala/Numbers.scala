package scunits

package object integer {

  trait Integer {
    type Self <: Integer
    type Succ <: Integer
    type Add[N <: Integer] <: Integer
    type Pred <: Integer
    type Sub[N <: Integer] <: Integer
    type Neg <: Integer
    type Mult[R <: Integer] <: Integer

    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] <: B

    type DimMag[B <: BaseQuantityLike, T <: Dims] <: Dims
  }

  trait NonNegInt extends Integer
  trait NonPosInt extends Integer
  trait NonZeroInt extends Integer {
    type DimMag[B <: BaseQuantityLike, T <: Dims] = DNelConst[B,Self,T]
  }
  trait NegInt extends NonPosInt with NonZeroInt {
    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = N
  }
  trait PosInt extends NonNegInt with NonZeroInt{
    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = P
  }
  // 4 * 2 = 2 + 3 * 2 = 2 + 2 + 2 * 2
  // 4#Mult[2] = 2#Add[3#Mult[2]]

  class SuccInt[P <: NonNegInt] extends PosInt {
    type Self = SuccInt[P]
    type Succ = SuccInt[SuccInt[P]]
    type Add[N <: Integer] = P#Add[N]#Succ
    type Pred = P
    type Sub[N <: Integer] = P#Sub[N]#Succ
    type Neg = P#Neg#Pred
    type Mult[R <: Integer] = R#Add[Pred#Mult[R]]
  }

  class PredInt[S <: NonPosInt] extends NegInt {
    type Self = PredInt[S]
    type Succ = S
    type Add[N <: Integer] = S#Add[N]#Pred
    type Pred = PredInt[PredInt[S]]
    type Sub[N <: Integer] = S#Sub[N]#Pred
    type Neg = S#Neg#Succ
    type Mult[R <: Integer] = R#Sub[Succ#Mult[R]]
  }

  final class _0 extends NonNegInt with NonPosInt {
    type Self = _0
    type Succ = SuccInt[_0]
    type Add[N <: Integer] = N
    type Pred = PredInt[_0]
    type Sub[N <: Integer] = N#Neg
    type Neg = _0
    type Mult[R <: Integer] = _0
    type BranchNegZeroPos[B, N<:B, Z<:B, P<:B] = Z

    type DimMag[B <: BaseQuantityLike, T <: Dims] = T
  }

  object Ops {
    type +[L <: Integer, R <: Integer] = L#Add[R]
    type -[L <: Integer, R <: Integer] = L#Sub[R]
    type *[L <: Integer, R <: Integer] = L#Mult[R]
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
}