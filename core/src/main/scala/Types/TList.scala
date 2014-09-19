package scunits.types

trait TList {
  type Base
  type Size <: NonNegInt
  type Of[BB >: Base] <: TListOf[BB]
  type Map[F[_ <: Base] <: Base] = MapTo[Base,F]
  type MapTo[BB,F[_ <: Base] <: BB] <: TListOf[BB]
  type Drop[N <: Integer] <: TListOf[Base]

  type ZipMap[R <: TListOf[Base], F[_ <: Base, _ <: Base] <: Base] <: TListOf[Base]
  protected type ZipMapNel[L <: TNelOf[Base], F[_ <: Base, _ <: Base] <: Base] <: TListOf[Base]

  // type Fill[With,To <: Integer] = To#IsPos#If[TListOf[With], TNil[With], With :: Fill[With,To#Pred]]
  type Append[R <: TListOf[Base]] <: TListOf[Base]
  type Set[I <: Integer,V <: Base] <: TListOf[Base]
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
  type Head = L
  type Tail = R#Of[R#Base]

  type This = Head :: Tail
  type Size = Tail#Size#Succ

  type Of[BB >: Base] = Head :: Tail#Of[BB]
  type MapTo[BB,F[_ <: Base] <: BB] = F[Head] :: Tail#MapTo[BB,F]
  type Drop[N <: Integer] = N#IsPos#If[TListOf[Base],Tail#Drop[N#Pred],This]
  
  type ZipMap[R <: TListOf[Base], F[_ <: Base, _ <: Base] <: Base] = R#ZipMapNel[This,F]
  protected type ZipMapNel[L <: TNelOf[Base], F[_ <: Base, _ <: Base] <: Base] = F[L#Head,Head] :: L#Tail#ZipMap[Tail,F]

  type Append[R <: TListOf[Base]] = Head :: Tail#Append[R]
  type Set[I <: Integer,V <: Base] = I#IsZero#If[TListOf[Base], V :: Tail, Head :: Tail#Set[I#Pred,V]]
}
trait TNelConst[B,H <: B,T <: TListOf[B]] extends (H :: T) {
  override type Base = B
}
trait TNil[B] extends TListOf[B] {
  type This = TNil[B]
  type Size = _0
  type MapTo[BB,F[_ <: B]] = TNil[BB]
  type Of[BB >: Base] = TNil[BB]
  type Drop[N <: Integer] = This

  type ZipMap[R <: TListOf[Base], F[_ <: Base, _ <: Base] <: Base] = This
  protected type ZipMapNel[L <: TNelOf[Base], F[_ <: Base, _ <: Base] <: Base] = This

  type Append[R <: TListOf[Base]] = R
  type Set[I <: Integer,V <: Base] = This

  type Test[N <: TNil[B]] = This
}