package scunits.types

import scunits._

trait Integer extends ComparableTo[Integer] {
  type This <: Integer
  type isZero <: Bool
  type isPos <: Bool
  type isNeg <: Bool
  type succ <: Integer  
  
  type add[N <: Integer] <: Integer
  type addNN[N <: NonNegInt] <: Integer
  type addNP[N <: NonPosInt] <: Integer

  type pred <: Integer
  type sub[N <: Integer] <: Integer
  type neg <: Integer


  type compare[R <: Integer] = (This#sub[R])#comp
  type comp <: Compared

  type PadDList[T <: Integer] <: DList
}

trait NonNegInt extends Integer {
  type isNeg = False
  type succ <: PosInt
  type addNN[N <: NonNegInt] <: NonNegInt  
}
trait NonPosInt extends Integer {
  type isPos = False
  type pred <: NegInt
  type addNP[N <: NonPosInt] <: NonPosInt
  type PadDList[T <: Integer] = T *: DNil
}
trait NonZeroInt extends Integer {
  type isZero = False
}
trait NegInt extends NonPosInt with NonZeroInt {
  type isNeg = True
  type succ <: NonPosInt
  type comp = Less
  type addNP[N <: NonPosInt] <: NegInt
}
trait PosInt extends NonNegInt with NonZeroInt {
  type isPos = True
  type pred <: NonNegInt
  type comp = Greater
  type addNN[N <: NonNegInt] <: PosInt
  type PadDList[T <: Integer] = i0 *: pred#PadDList[T]
}

class SuccInt[P <: NonNegInt] extends PosInt {
  type This = SuccInt[P]
  type succ = SuccInt[SuccInt[P]]
  type add[N <: Integer] = P#add[N]#succ
  type addNN[N <: NonNegInt] = P#addNN[N]#succ
  type addNP[N <: NonPosInt] = add[N]
  type pred = P
  type sub[N <: Integer] = P#sub[N]#succ
  type neg = P#neg#pred
}

class PredInt[S <: NonPosInt] extends NegInt {
  type This = PredInt[S]
  type succ = S
  type add[N <: Integer] = S#add[N]#pred
  type addNN[N <: NonNegInt] = add[N]
  type addNP[N <: NonPosInt] = S#addNP[N]#pred 
  type pred = PredInt[PredInt[S]]
  type sub[N <: Integer] = S#sub[N]#pred
  type neg = S#neg#succ
}

final class i0 extends NonNegInt with NonPosInt {
  type This = i0
  type isZero = True
  type succ = SuccInt[i0]
  type add[N <: Integer] = N
  type addNN[N <: NonNegInt] = N
  type addNP[N <: NonPosInt] = N
  type pred = PredInt[i0]
  type sub[N <: Integer] = N#neg
  type neg = i0
  type comp = Equal  
}