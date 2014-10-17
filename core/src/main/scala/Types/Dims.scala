package scunits.types

trait Quant

trait Dims2 {
  type accessors <: AList
  type quant <: accessors#quant

  type neg = Dims2Const[accessors, accessors#neg[quant]#apply]
}
trait Dims2Const[As <: AList, Q <: As#quant] extends Dims2 {
  type accessors = As
  type quant = Q
}
  