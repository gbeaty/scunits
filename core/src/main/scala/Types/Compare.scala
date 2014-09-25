package scunits.types

trait Compared {
  type isLess <: Bool
  type isEqual <: Bool
  type isGreater <: Bool

  type isLessOrEqual = isLess || isEqual
  type isGreaterOrEqual = isGreater || isEqual
}
trait EqualOrLess extends Compared
trait EqualOrGreater extends Compared
trait NotEqual extends Compared
trait Less extends EqualOrLess with NotEqual {
  type isLess = True
  type isEqual = False
  type isGreater = False
}
trait Equal extends EqualOrLess with EqualOrGreater {
  type isLess = False
  type isEqual = True
  type isGreater = False
}
trait Greater extends EqualOrGreater with NotEqual {
  type isLess = False
  type isEqual = False
  type isGreater = True
}

trait Comparable {
  type to <: ComparableTo[to]
  type compare[R <: to] <: Compared
}
trait ComparableTo[W <: ComparableTo[W]] extends Comparable {
  type to = W  
}