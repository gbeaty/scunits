package scunits.types

trait AList {
  type self <: AList
  type set <: Accessor
  type quant <: Quant   

  protected type bugNeg[L <: quant] <: quant
  protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] <: Dims2
  
  type dimlessQuant <: quant
  type append[As <: AList] <: AList

  class op[L <: quant, R <: quant] {
    type apply[O[_ <: Integer, _ <: Integer] <: Integer] = bugOp[L,R,O]
  }
  class neg[L <: quant] {
    type apply = bugNeg[L]
  }

  type dimless = Dims2Const[self, self#dimlessQuant]
}
trait ANel extends AList {
  type head <: Accessor
  type tail <: AList
}
trait ::[L <: Accessor, R <: AList] extends ANel {
  type head = L
  type tail = R
  type self = head :: tail
  type set = head with tail#set
  type quant = head#of with tail#quant
  type dimlessQuant = head#set[tail#dimlessQuant, _0]
  type append[As <: AList] = head :: tail#append[As]

  protected type bugNeg[L <: quant] = head#set[tail#bugNeg[L], head#get[L]#Neg]

  protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] = ({
    type exp = O[head#get[L], head#get[R]]
    type rem = tail#bugOp[head#set[L,_0], head#set[R,_0], O]
    type res = exp#isZero#branch[Dims2, rem, Dims2Const[head :: rem#accessors, head#set[rem#quant,exp]]]
  })#res
}
trait ANil extends AList {
  type self = ANil
  type set = Accessor
  type quant = Quant
  type dimlessQuant = Quant
  type append[As <: AList] = As

  protected type bugNeg[L <: quant] = Quant
  protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] = Dimless
}