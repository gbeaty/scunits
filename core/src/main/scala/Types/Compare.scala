package scunits.types

trait Compared {
  type IsLess <: Bool
  type IsEqual <: Bool
  type IsGreater <: Bool

  type IsLessOrEqual = IsLess || IsEqual
  type IsGreaterOrEqual = IsGreater || IsEqual
}
trait EqualOrLess extends Compared
trait EqualOrGreater extends Compared
trait NotEqual extends Compared
trait Less extends EqualOrLess with NotEqual {
  type IsLess = True
  type IsEqual = False
  type IsGreater = False
}
trait Equal extends EqualOrLess with EqualOrGreater {
  type IsLess = False
  type IsEqual = True
  type IsGreater = False
}
trait Greater extends EqualOrGreater with NotEqual {
  type IsLess = False
  type IsEqual = False
  type IsGreater = True
}

trait Comparable {
  type With <: ComparableWith[With]
  type Compare[R <: With] <: Compared
}
trait ComparableWith[W <: ComparableWith[W]] extends Comparable {
  type With = W  
}