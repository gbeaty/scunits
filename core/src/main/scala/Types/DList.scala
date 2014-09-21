package scunits.types

trait DList {
  type Set[I <: NonNegInt, To <: NonZeroInt] = SetInt[I,To] // Seems to accept To <: NonNegInt ??
  protected type SetInt[I <: Integer, To <: Integer] <: DList

  type Empty <: Bool
  type Head <: Integer
  type Tail <: DList
  type Get[I <: NonNegInt] = GetInt[I]  
  protected type GetInt[I <: Integer] <: Integer

  type Neg <: DList
  
  type Mult[R <: DList] = Op[R,+]
  type Div[R <: DList] = Op[R,-]
  type Op[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] <: DList
  protected type OpNel[L <: DNel, O[_ <: Integer, _ <: Integer] <: Integer] <: DNel
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] <: DList

  type IsPadding <: Bool
  type TruncZeros <: DList
}
trait DNel extends DList {
  type Empty = False
  type Head <: Integer
  type Tail <: DList  
}

trait *:[H <: Integer, T <: DList] extends DNel {
  type Head = H
  type Tail = T
  type This = Head *: Tail

  type Neg = Head#Neg *: Tail#Neg
  type SetInt[I <: Integer, To <: Integer] = I#IsPos#If[DList, Head *: Tail#Set[I#Pred,To], (To *: Tail)]

  protected type GetInt[I <: Integer] = I#IsZero#If[Integer, Head, Tail#GetInt[I#Pred]]
  
  type Op[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNel[This,O]#TruncZeros
  protected type OpNel[L <: DNel, O[_ <: Integer, _ <: Integer] <: Integer] = O[L#Head, Head] *: L#Tail#Op[Tail,O]
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = O[i0,Head] *: Tail#OpNil[O]

  type IsPadding = Head#IsZero && Tail#IsPadding
  type TruncZeros = IsPadding#If[DList, DNil, Head *: Tail#TruncZeros]
}
trait DNil extends DList {
  type Empty = True
  type Head = i0
  type Tail = DNil
  type SetInt[I <: Integer, To <: Integer] = To#IsZero#If[DList,DNil,I#PadDList[To]]
  type Neg = DNil

  protected type GetInt[I <: Integer] = i0

  type Op[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNil[O]
  protected type OpNel[L <: DNel, O[_ <: Integer, _ <: Integer] <: Integer] = L#Head *: L#Tail
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = DNil

  type IsPadding = True
  type TruncZeros = DNil
}