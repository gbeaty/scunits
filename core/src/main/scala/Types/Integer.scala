package scunits.types

import scunits._

sealed trait Integer {
  type succ <: Integer  
  
  type add[N <: Integer] <: Integer

  type pred <: Integer
  type sub[N <: Integer] <: Integer
  type neg <: Integer

  type mult[R <: Integer] <: Integer

  type ifZero[B, T <: B, E[_ <: NonZeroInt] <: B] <: B
  type dimsNzAdd[D <: Dims, B <: BaseQuantity] <: Dims
  type withNonZero[A,W] <: A
}

sealed trait NonNegInt extends Integer {
  type neg <: NonPosInt
  type succ <: PosInt
}
sealed trait NonPosInt extends Integer {
  type neg <: NonNegInt
  type pred <: NegInt
}
sealed trait NonZeroInt extends Integer {
  type neg <: NonZeroInt
}
sealed trait NegInt extends NonPosInt with NonZeroInt {
  type neg <: PosInt
  type succ <: NonPosInt
}
sealed trait PosInt extends NonNegInt with NonZeroInt {
  type neg <: NegInt
  type pred <: NonNegInt
}

sealed class ++[P <: NonNegInt] extends PosInt {
  type succ = ++[++[P]]
  type add[N <: Integer] = P#add[N#succ]
  type pred = P
  type sub[N <: Integer] = P#sub[N#pred]
  type neg = P#neg#pred
  type mult[R <: Integer] = R#add[pred#mult[R]]
  type ifZero[B, T <: B, E[_ <: NonZeroInt] <: B] = E[++[P]]
  type dimsNzAdd[D <: Dims, B <: BaseQuantity] = DimsConst[D#bases with B#of, D#values with B#set[++[P]]]
  type withNonZero[A,W] = A with W
}

sealed class --[S <: NonPosInt] extends NegInt {
  type succ = S
  type add[N <: Integer] = S#add[N#pred]
  type pred = --[--[S]]
  type sub[N <: Integer] = S#sub[N#succ]
  type neg = S#neg#succ
  type mult[R <: Integer] = R#sub[succ#mult[R]]
  type ifZero[B, T <: B, E[_ <: NonZeroInt] <: B] = E[--[S]]
  type dimsNzAdd[D <: Dims, B <: BaseQuantity] = DimsConst[D#bases with B#of, D#values with B#set[--[S]]]
  type withNonZero[A,W] = A with W
}

sealed class _0 extends NonNegInt with NonPosInt {
  type succ = ++[_0]
  type add[N <: Integer] = N
  type pred = --[_0]
  type sub[N <: Integer] = N#neg
  type neg = _0
  type mult[R <: Integer] = _0
  type ifZero[B, T <: B, E[_ <: NonZeroInt] <: B] = T
  type dimsNzAdd[D <: Dims, B <: BaseQuantity] = D
  type withNonZero[A,W] = A
}