package scunits.types

trait TList {
  type Base
  type Size <: NonNegInt
  type Of[BB >: Base] <: TListOf[BB]
  type Map[F[_ <: Base] <: Base] = MapTo[Base,F]
  type MapTo[BB,F[_ <: Base] <: BB] <: TListOf[BB]  
  type ModifyOne[If[_ <: TNelOf[Base]] <: Bool, With[_ <: TNelOf[Base]] <: TListOf[Base]] <: TListOf[Base]
  type MapListTo[BB,F[_ <: TNelOf[Base]] <: TListOf[BB]] <: TListOf[BB]
  type Drop[N <: Integer] <: TListOf[Base]
  // type MapList[F[_ <: TNelOf[Base]] <: TListOf[Base]] = MapListTo[Base,F]
  // type InsertIf[El <: Base, If[_ <: TListOf[Base]] <: Bool] <: TListOf[Base]
}
trait TListOf[B] extends TList {
  type Base = B
}
trait TNel extends TList {
  type Head <: Base
  type Tail <: TListOf[Base]
}
trait TNelOf[B] extends TListOf[B] with TNel
trait ::[L <: R#Base, R <: TList] extends TNelOf[R#Base] {
  type This = ::[L,R]
  type Size = R#Size#Succ
  type Head = L
  type Tail = R#Of[R#Base]
  type Of[BB >: Base] = L :: R#Of[BB]
  type MapTo[BB,F[_ <: Base] <: BB] = F[Head] :: Tail#MapTo[BB,F]
  type ModifyOne[If[_ <: TNelOf[Base]] <: Bool, With[_ <: TNelOf[Base]] <: TListOf[Base]] =
    If[This]#If[TListOf[Base], With[This], Head :: Tail#ModifyOne[If, With]]
  type Drop[N <: Integer] = N#IsPos#If[TListOf[Base],Tail#Drop[N#Pred],This]

  // type InsertIf[El <: Base, If[_ <: TListOf[Base]] <: Bool] = If[This]#If[TListOf[Base], El :: This, Tail#InsertIf[El,If]]
}
trait TNil[B] extends TListOf[B] {
  type This = TNil[B]
  type Size = _0
  type MapTo[BB,F[_ <: B]] = TNil[BB]
  type Of[BB >: Base] = TNil[BB]
  type ModifyOne[If[_ <: TNelOf[Base]] <: Bool, With[_ <: TNelOf[Base]] <: TListOf[Base]] = TNil[B]
  // type InsertIf[El <: Base, If[_ <: TListOf[Base]] <: Bool] = If[This]#If[TListOf[Base], El :: This, This]
  type Drop[N <: Integer] = This
}

trait SortedTList[B <: ComparableWith[B]] extends TListOf[B] {
  // type InsertOrMerge[E <: B] = 
}

trait Quants extends TListOf[Quantity]
/*trait Dim {
  type Quant <: Quantity
  type Exp <: Integer
  type Neg = Quant ^ Exp#Neg
  type Mult[R <: DimOf[Quant]] = Quant ^ (Exp + R#Exp)
}
trait DimOf[Q <: Quantity] extends Dim {
  type Quant = Q
}
trait ^[L <: Quantity, R <: Integer] extends DimOf[L] {
  type Exp = R
}
trait Dims extends TListOf[Dim] {
  type ToQuant[D <: Dim] = D#Quant
  type NegDim[D <: Dim] = D#Neg
  type Quants = MapTo[Quantity,ToQuant]
  type Neg = Map[NegDim]
}*/