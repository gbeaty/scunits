package scunits

package object bool {
  trait Bool {
    type Not <: Bool
    type Or[R <: Bool] <: Bool
    type And[R <: Bool] <: Bool
    type Xor[R <: Bool] = Or[R]#And[And[R]#Not]
    type If[Then,Else]
    type Ifs[B,Then <: B,Else <: B] <: B
  }
  final class True extends Bool {
    type Not = False
    type Or[R <: Bool] = True
    type And[R <: Bool] = R    
    type If[Then,Else] = Then
    type Ifs[B,Then <: B,Else <: B] = Then
  }
  final class False extends Bool {
    type Not = True
    type Or[R <: Bool] = R
    type And[R <: Bool] = False
    type If[Then,Else] = Else
    type Ifs[B,Then <: B,Else <: B] = Else
  }

  implicitly[True#Not =:= False]
  implicitly[False#Not =:= True]

  implicitly[True#Or[True] =:= True]
  implicitly[True#Or[False] =:= True]
  implicitly[False#Or[True] =:= True]
  implicitly[False#Or[False] =:= False]

  implicitly[True#And[True] =:= True]
  implicitly[True#And[False] =:= False]
  implicitly[False#And[True] =:= False]
  implicitly[False#And[False] =:= False]

  implicitly[True#Xor[True] =:= False]
  implicitly[True#Xor[False] =:= True]
  implicitly[False#Xor[True] =:= True]
  implicitly[False#Xor[False] =:= False]

  implicitly[True#If[Int,Double] =:= Int]
  implicitly[False#If[Int,Double] =:= Double]

  trait Letter
  trait A extends Letter
  trait B extends Letter
  implicitly[True#If[A,B] <:< Letter]
  implicitly[False#If[A,B] <:< Letter]
}