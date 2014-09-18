package scunits.types

object TListTests {
  trait MetaBase
  trait Base extends MetaBase { type ToNum <: Integer; type IsA <: Bool; type IsB <: Bool; type IsC <: Bool }
  trait A extends Base { type ToNum = _1; type IsA = True; type IsB = False; type IsC = False }
  trait B extends Base { type ToNum = _2; type IsA = False; type IsB = True; type IsC = False }
  trait C extends Base { type ToNum = _3; type IsA = False; type IsB = False; type IsC = True }

  type BC = B :: C :: TNil[Base]
  type ABC = A :: BC
  type OneTwo = _1 :: _2 :: TNil[Integer]
  type OneTwoThree = _1 :: _2 :: _3 :: TNil[Integer]
  type ToNum[B <: Base] = B#ToNum
  type ToZero[B <: Base] = _0

  type JustA = A :: TNil[Base]

  implicitly[ABC#Of[MetaBase] =:= (A :: B :: C :: TNil[MetaBase])]

  implicitly[TNil[Any]#MapTo[Integer,ToZero] =:= TNil[Integer]]
  implicitly[ABC#MapTo[Integer,ToNum] =:= (_1 :: _2 :: _3 :: TNil[Integer])]

  implicitly[ABC#Size#IsZero =:= False]
  implicitly[TNil[Any]#Size#IsZero =:= True]

  type Test = OneTwo#Head == _1
  type IfOne[El <: TNelOf[Integer]] = (El#Head#Sub[_1]#IsZero)
  type AddOne[El <: TNelOf[Integer]] = (El#Head + _1) :: El#Tail
  // val a: OneTwo#ModifyOne[IfOne,AddOne] = 1
  // implicitly[OneTwo#ModifyOne[IfOne,AddOne] =:= (_2 :: _2 :: TNil[Integer])]

  implicitly[OneTwo#Size#Compare[OneTwoThree#Size] =:= Less]
  implicitly[OneTwo#Size#Compare[OneTwo#Size] =:= Equal]
  implicitly[OneTwoThree#Size#Compare[OneTwo#Size] =:= Greater]

  // Test drops:
  implicitly[ABC#Drop[_5#Neg] =:= ABC]
  implicitly[ABC#Drop[_0] =:= ABC]
  implicitly[ABC#Drop[_1] =:= BC]
  implicitly[ABC#Drop[_2] =:= (C :: TNil[Base])]
  implicitly[ABC#Drop[_3] =:= TNil[Base]]
  implicitly[ABC#Drop[_9] =:= TNil[Base]]

  type Sublist[L <: TList, RSize <: NonNegInt] = L#Drop[L#Size - RSize]
  implicitly[Sublist[ABC,BC#Size] =:= BC]
  implicitly[Sublist[ABC,ABC#Size] =:= ABC]
  implicitly[Sublist[ABC,TNil[Base]#Size] =:= TNil[Base]]

  type IsSublist[L <: TList, R <: Sublist[L,RSize], RSize <: R#Size] = True
  implicitly[IsSublist[ABC,BC,BC#Size] =:= True]

  /*TListTests.IfOne[_1 :: _2 :: TNil[Integer]]#If[
    TListOf[Integer],
    TListTests.AddOne[_1 :: _2 :: TNil[Integer]],
    TListTests.IfOne[_2 :: TNil[Integer]]#If[
      TListOf[Integer],
      TListTests.AddOne[_2 :: TNil[Integer]],
      TNil[Integer]
    ]
  ] =:= _2 :: _2 :: TNil[Integer]*/

  /*
  Integer.this.Sub[_1]#IsZero#If[
    TListOf[Integer],
    _2 :: _2 :: TNil[Integer],
    Integer.this.Sub[_1]#IsZero#If[
      TListOf[Integer],
      _3 :: TNil[Integer],
      TNil[Integer]
    ]
  ]*/
}