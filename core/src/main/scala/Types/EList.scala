package scunits.types

trait EList {
  type Set[I <: NonNegInt, To <: NonZeroInt] = SetInt[I,To] // Seems to accept To <: NonNegInt ??
  protected type SetInt[I <: Integer, To <: Integer] <: EList

  type Empty <: Bool
  type Head <: Integer
  type Tail <: EList
  type Get[I <: NonNegInt] = GetInt[I]  
  protected type GetInt[I <: Integer] <: Integer

  type neg <: EList
  
  type mult[R <: EList] = Op[R,+]
  type div[R <: EList] = Op[R,-]
  type Op[R <: EList, O[_ <: Integer, _ <: Integer] <: Integer] <: EList
  protected type OpNel[L <: ENel, O[_ <: Integer, _ <: Integer] <: Integer] <: ENel
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] <: EList

  type IsPadding <: Bool
  type TruncZeros <: EList
}
trait ENel extends EList {
  type Empty = False
  type Head <: Integer
  type Tail <: EList  
}

trait *:[H <: Integer, T <: EList] extends ENel {
  type Head = H
  type Tail = T
  type This = Head *: Tail

  type neg = Head#neg *: Tail#neg
  type SetInt[I <: Integer, To <: Integer] = I#isPos#If[EList, Head *: Tail#Set[I#pred,To], (To *: Tail)]

  protected type GetInt[I <: Integer] = I#isZero#If[Integer, Head, Tail#GetInt[I#pred]]
  
  type Op[R <: EList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNel[This,O]#TruncZeros
  protected type OpNel[L <: ENel, O[_ <: Integer, _ <: Integer] <: Integer] = O[L#Head, Head] *: L#Tail#Op[Tail,O]
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = O[i0,Head] *: Tail#OpNil[O]

  type IsPadding = Head#isZero && Tail#IsPadding
  type TruncZeros = IsPadding#If[EList, ENil, Head *: Tail#TruncZeros]
}
trait ENil extends EList {
  type Empty = True
  type Head = i0
  type Tail = ENil
  type SetInt[I <: Integer, To <: Integer] = To#isZero#If[EList,ENil,I#PadEList[To]]
  type neg = ENil

  protected type GetInt[I <: Integer] = i0

  type Op[R <: EList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNil[O]
  protected type OpNel[L <: ENel, O[_ <: Integer, _ <: Integer] <: Integer] = L#Head *: L#Tail
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = ENil

  type IsPadding = True
  type TruncZeros = ENil
}