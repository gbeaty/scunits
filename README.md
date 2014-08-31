# scunits

(or Yet Another Scala Units Library)

scunits is an experimental library for type-checking dimensional quantities and handling units of measure in scala. Exponents of base quantities are stored at the type-level in recursive list structures similar to shapeless's hlists, but without any run-time instances. To keep things simple, all measurements are stored as Doubles with SI values for their appropriate unit.

## Features
- All measurements are Value Classes wrapping Doubles
- Users may add new base quantities
- Dimensions (`Dims`), units (`UnitM`) and measurements (`Measure`) may be composed by multiplication and division
- All SI prefixes
- Array wrappers
- < 100k jar file size (for now...)
- No dependencies

Unfortunately dimension type signatures are rather unreadable.

## Todo
- BigInt support
- More units
- Verify compilation times are kept reasonable
- Dims#Pow, Measure.pow and UnitM.pow functions
- Logarithmic scale units

## Using scunits
See: https://github.com/gbeaty/scunits/blob/master/test/src/test/scala/Examples.scala

## Getting scunits
scunits is not curretly in a maven repository. You'll have to `git clone https://github.com/gbeaty/scunits.git`, run `sbt publish-local`, then add `"org.scunits" %% "core" % "0.0.1"` to your project dependencies.