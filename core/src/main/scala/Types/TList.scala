package scunits.types

trait QList {
  type Dimless <: DList  
}
trait QNel extends QList {
  type Head <: scunits2.Quantity
  type Tail <: QList

  type Dimless = i0 ::: Tail#Dimless

}
trait ::[L <: scunits2.Quantity,R <: QList] extends QNel {
  type Head = L
  type Tail = R
}
trait QNil extends QList {
  type Dimless = DNil
}

trait DList {
  type Set[I <: Integer, To <: Integer] <: DList
  type Neg <: DList
  
  type Mult[R <: DList] = Op[R,+]
  type Div[R <: DList] = Op[R,-]
  type Op[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] <: DList
  protected type OpNel[L <: DNel, O[_ <: Integer, _ <: Integer] <: Integer] <: DNel
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] <: DList
  // type Zeros[N <: Integer] = N#IsPos#If[DList, i0 ::: Zeros[N#Pred], DNil]
}
trait DNel extends DList {  
  type Head <: Integer
  type Tail <: DList
  type This = Head ::: Tail

  type Neg = Head#Neg ::: Tail#Neg
  type Set[I <: Integer, To <: Integer] = I#IsPos#If[DList, Head ::: Tail#Set[I#Pred,To], To ::: Tail]
  
  type Op[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNel[This,O]
  protected type OpNel[L <: DNel, O[_ <: Integer, _ <: Integer] <: Integer] = O[L#Head, Head] ::: L#Tail#Op[Tail,O]
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = O[i0,Head] ::: Tail  
}
trait :::[H <: Integer, T <: DList] extends DNel {
  type Head = H
  type Tail = T
}
trait DNil extends DList {
  type Set[I <: Integer, To <: Integer] = DNil//I#IsPos#If[DList, i0 ::: DNil#Set[I#Pred,To], To ::: DNil]
  type Neg = DNil
  type Op[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNil[O]
  protected type OpNel[L <: DNel, O[_ <: Integer, _ <: Integer] <: Integer] = L#Head ::: L#Tail
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = DNil
}

/*
trait TList {
  type Base
  type This <: TListOf[Base]
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
  final type Base = B
}
trait TNel extends TList {
  type Head <: Base
  type Tail <: TListOf[Base]
}
trait TNelOf[B] extends TListOf[B] with TNel
class ::[L <: R#Base, R <: TList] extends TNelOf[R#Base] {    
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
class TNil[B] extends TListOf[B] {
  type This = TNil[B]
  type Size = _0
  type MapTo[BB,F[_ <: B]] = TNil[BB]
  type Of[BB >: Base] = TNil[BB]
  type Drop[N <: Integer] = This

  type ZipMap[R <: TListOf[Base], F[_ <: Base, _ <: Base] <: Base] = This
  protected type ZipMapNel[L <: TNelOf[Base], F[_ <: Base, _ <: Base] <: Base] = This

  type Append[R <: TListOf[Base]] = R
  type Set[I <: Integer,V <: Base] = This

  type Test[N <: Base] = This
}*/