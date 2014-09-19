package scunits.types

trait QList
trait QNel {
  type Head <: scunits2.Quantity
  type Tail <: QList
}
trait QNelOf[L <: scunits2.Quantity,R <: QList] extends QNel {
  type Head = L
  type Tail = R
}
trait QNil extends QList

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
}

class TesterOf[A] {
  final type Do[N <: A] = N
  type Do2[A] = A
}
class AbTester {
  type A
  type Do[N <: A] = N
}
class AliasTester {
  type A = Int
  type Do[N <: A] = N
}
class AbTester3 extends AbTester {
  type A = Int
}
class CoTester[A] {
  type Do[N <: A] = A
}
final class SubTester extends TesterOf[Int]

class TestContainer[A] {
  class Contained {
    type Do[N <: A] = N
  }
}

object Test {
  // type TestOf = TesterOf[Int]#Do[Int]
  val testerOf = new TesterOf[Int]
  type inTest = testerOf.Do[Int]
  // type AbTest = (AbTester{type A=Int})#Do[Int]  
  type AliasTest = AliasTester#Do[Int]
  // type AbTest3 = AbTester3#Do[Int]
  // type subTest = SubTester#Do[Int]
  // type CoTest = CoTester[Int]#Do[Int]

  val intContainer = new TestContainer[Int]
  val inContained = new intContainer.Contained
  // type ContainerTest = intContainer.Contained#Do[Int]
  type ContainedTest = inContained.Do[Int]

  object ContainerObj extends TestContainer[Int] {
    type ObjTest = Contained#Do[Int]
  }
}