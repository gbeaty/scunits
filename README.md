# scunits

(or Yet Another Scala Units Library)

scunits is an experimental, extensible library for type-checking dimensional quantities and handling units of measure in scala. Exponents of base quantities are stored at the type-level in recursive list structures similar to shapeless's hlists, but without any run-time instances. All measurements are stored as Doubles of their appropriate SI unit and only converted to a specific unit when needed.

## Features
- All measurements are Value Classes wrapping Doubles
- Users may add new base quantities
- Dimensions (`Dims`), units (`UnitM`) and measurements (`Measure`) may be composed by multiplication and division
- All SI prefixes (kilo, centi, etc.)
- Array wrappers (`ArrayM`) for unboxed arrays
- < 100k jar file size (for now...)
- No dependencies
- Elementary algebra on abstract `Measure` types

## Todo
- BigInt support
- More units
- Verify compilation times are kept reasonable.
- Dims#Pow, Measure.pow and UnitM.pow functions
- Logarithmic scale units
- Find a small, more performant type-level integer library.
- If possible, make the type signatures readable.

## Using scunits
See: https://github.com/gbeaty/scunits/blob/master/test/src/test/scala/Examples.scala

## Getting scunits
scunits is not curretly in a maven repository. You'll have to `git clone https://github.com/gbeaty/scunits.git`, run `sbt publish-local`, then add `"org.scunits" %% "core" % "0.0.1"` to your project dependencies.