# scunits

(or Yet Another Scala Units Library)

scunits is an experimental, extensible library for type-checking dimensional quantities and handling units of measure in scala. Exponents of base quantities are stored at the type-level in recursive list structures similar to shapeless's hlists, but without any run-time instances. All measurements are stored as Doubles of their appropriate SI unit and only converted to a specific unit when needed.

## Features
- All measurements (`Measure`) are Value Classes wrapping Doubles. Performance is identical to Doubles once the JIT is warm.
- Users may add new base quantities
- Dimensions (`Dims`), units (`UnitM`) and measurements (`Measure`) may be composed by multiplication and division
- All SI prefixes (kilo, centi, etc.)
- Array wrappers (`ArrayM`) for unboxed arrays
- < 200k jar file size (for now...)
- No dependencies
- Elementary algebra on abstract `Measure` types

## Todo
- Other numeric (BigDecimal, Int, etc.) support for Measures.
- Unit conversion tests
- Verify compilation times are kept reasonable.
- Logarithmic scale units
- If possible, make the type signatures readable.
- Add support for better BaseQuantity composition.

## Using scunits
See: https://github.com/gbeaty/scunits/blob/master/test/src/test/scala/Examples.scala

## Getting scunits
scunits is not curretly in a maven repository as I don't consider it ready for general use. You'll have to `git clone https://github.com/gbeaty/scunits.git`, run `sbt publish-local`, then add `"org.scunits" %% "core" % "0.0.1-SNAPSHOT"` to your project dependencies.