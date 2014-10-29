package scunits.types

import scunits._

sealed trait Integer {
  type succ <: Integer  
  type add[N <: Integer] <: Integer
  type pred <: Integer
  type sub[N <: Integer] <: Integer
  type neg <: Integer
  type mult[R <: Integer] <: Integer
}

sealed trait NonNegInt extends Integer {
  type succ <: PosInt
}
sealed trait NonPosInt extends Integer {
  type pred <: NegInt
}
sealed trait NonZeroInt extends Integer
sealed trait NegInt extends NonPosInt with NonZeroInt {
  type succ <: NonPosInt
}
sealed trait PosInt extends NonNegInt with NonZeroInt {
  type pred <: NonNegInt
}

sealed class SuccInt[P <: NonNegInt] extends PosInt {
  type succ = SuccInt[SuccInt[P]]
  type add[N <: Integer] = P#add[N#succ]
  type pred = P
  type sub[N <: Integer] = P#sub[N#pred]
  type neg = P#neg#pred
  type mult[R <: Integer] = R#add[pred#mult[R]]
}

sealed class PredInt[S <: NonPosInt] extends NegInt {
  type succ = S
  type add[N <: Integer] = S#add[N#pred]
  type pred = PredInt[PredInt[S]]
  type sub[N <: Integer] = S#sub[N#succ]
  type neg = S#neg#succ
  type mult[R <: Integer] = R#sub[succ#mult[R]]
}

sealed class _0 extends NonNegInt with NonPosInt {
  type succ = SuccInt[_0]
  type add[N <: Integer] = N
  type pred = PredInt[_0]
  type sub[N <: Integer] = N#neg
  type neg = _0
  type mult[R <: Integer] = _0
}