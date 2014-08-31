# scunits

(or Yet Another Scala Units Library)

scunits is an experimental library for handling measurements, units of measure, quantities and dimensions in scala. Exponents of base quantities are tracked at compile time by type-level recursive list structures similar to shapeless's hlists, but without any run-time instances. To keep things simple, all measurements are stored as Doubles with SI values for their appropriate unit.

## Features
- All measurements are Value Classes wrapping Doubles.
- Users may add new base quantities.
- Both dimensions and units may be composed.
- All SI prefixes.
- Array wrappers
- < 100k jar file size (for now...)
- No dependencies

Unfortunately the Measure type signatures are rather unreadable.

## Todo
- BigInt support
- More units
- Compile time testing
- A measure.pow function
- Logarithmic scale units 

## Using scunits
See: 

## Getting scunits
scunits is not curretly in a maven repository. You'll have to `git clone https://github.com/gbeaty/scunits.git`, run `sbt publish-local`, then add `"org.scunits" %% "core" % "0.0.1"` to your project dependencies.