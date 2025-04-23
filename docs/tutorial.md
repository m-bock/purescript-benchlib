<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Benchmarks](#benchmarks)
  - [Preparation Functions](#preparation-functions)
  - [Benchmark Definitions](#benchmark-definitions)
  - [Options](#options)
- [Groups](#groups)
  - [Options](#options-1)
  - [Normalization](#normalization)
- [Suites](#suites)
  - [Runners](#runners)
  - [Reporters](#reporters)
    - [Console](#console)
    - [HTML](#html)
    - [JSON](#json)
- [Full Examples](#full-examples)
  - [Basic](#basic)
- [Effectful Benchmarks](#effectful-benchmarks)
- [Appendix](#appendix)
  - [Module Imports](#module-imports)
  - [Convenience Functions](#convenience-functions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Benchmarks

In this tutorial, we'll use **benchlib** to compare the runtime performance of computing the length of three different PureScript collections: `Array`, `List`, and `String`.

Here are the type signatures of the functions we'll benchmark:

<!-- PD_START:purs
inline: true
pick:
  - tag: signature_or_foreign
    name: length
    filePath: .spago/p/arrays-7.3.0/src/Data/Array.purs
    prefix: '- '
  - tag: signature_or_foreign
    name: length
    filePath: .spago/p/lists-7.0.0/src/Data/List.purs
    prefix: '- '
  - tag: signature_or_foreign
    name: length
    filePath: .spago/p/strings-6.0.1/src/Data/String/CodePoints.purs
    prefix: '- '
split: true
-->

- `length :: forall a. Array a -> Int`
- `length :: forall a. List a -> Int`
- `length :: String -> Int`

<!-- PD_END -->

Each function computes the number of elements in its respective collection type.

## Preparation Functions

Before we define our actual benchmarks, we need to create functions that prepare input data of a given size.

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - Size
-->

```purescript
type Size = Int
```

<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Test/Doc.purs
pick:
  - prepareCharArray
  - prepareCharList
  - prepareString
-->

```purescript
prepareCharArray :: Size -> Array Char
prepareCharArray size = Array.replicate size 'x'

prepareCharList :: Size -> List Char
prepareCharList size = List.replicate size 'x'

prepareString :: Size -> String
prepareString size = String.fromCharArray (Array.replicate size 'x')
```

<!-- PD_END -->

Each function takes a `Size` (an alias for `Int`) and returns a uniformly filled collection of that size using the character `'x'`.

## Benchmark Definitions

The following is the type signature of the `bench_` function, which is used to define a benchmark.

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - tag: signature
    name: bench_
-->

```purescript
bench_ :: forall a b. String -> BenchBaseOpts a b -> Bench Unit Unit
```

<!-- PD_END -->

As you can see, the `bench_` function takes a string (the name of the benchmark) and a `BenchBaseOpts` record. The `BenchBaseOpts` record is defined as follows:

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - BenchBaseOpts
-->

```purescript
type BenchBaseOpts a b =
  { prepare :: Size -> a
  , run :: a -> b
  }
```

<!-- PD_END -->

The return type of `bench_` is a `Bench Unit Unit`. We'll come back to the `Bench` type later. the two `Unit` parameters signalize that input and output of the benchmark are not obvervable from the outside. They will make more sense when normalization is introduced.

Let's have a look the three benchmarks we want to define:

<!-- PD_START:purs
filePath: test/Test/Doc.purs
pick:
  - bench1
  - bench2
  - bench3
-->

```purescript
bench1 :: Bench Unit Unit
bench1 = BL.bench_
  "Length of Array of Char"
  { prepare: prepareCharArray
  , run: Array.length
  }

bench2 :: Bench Unit Unit
bench2 = BL.bench_
  "Length of List of Char"
  { prepare: prepareCharList
  , run: List.length
  }

bench3 :: Bench Unit Unit
bench3 = BL.bench_
  "Length of String"
  { prepare: prepareString
  , run: String.length
  }
```

<!-- PD_END -->

Since they all have the same type, they can be easily grouped together as we will see in one of the next sections.

## Options

But first let's have a look at a more powerful version of the `bench_` function.
The following is the type signature of the `bench` function, which is used to define a benchmark with options.

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - tag: signature
    name: bench
-->

```purescript
bench
  :: forall a' b' a b
   . String
  -> (BenchOpts Unit Unit a b -> BenchOpts a' b' a b)
  -> BenchBaseOpts a b
  -> Bench a' b'
```

<!-- PD_END -->

First argument is again the name of the benchmark, the second argument is a function that takes a default `BenchOpts` record and returns a modified `BenchOpts` record. The third argument is the same as before, a mandatory `BenchBaseOpts` record.

The `BenchOpts` record is defined as follows:

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - BenchOpts
-->

```purescript
type BenchOpts a' b' a b =
  { iterations :: Int
  , normIn :: a -> a'
  , normOut :: b -> b'
  }
```

<!-- PD_END -->

The `iterations` field specifies how many times the benchmark should be run in order to get a reliable result. The `normIn` and `normOut` fields are functions that normalize the input and output of the benchmark.

# Groups

A group is a collection of benchmarks that should be compared to each other. For example, we can group the three benchmarks we defined earlier into a single group called "Length operations".

<!-- PD_START:purs
filePath: test/Test/Doc.purs
pick:
  - group1
-->

```purescript
group1 :: Group
group1 = BL.group_
  "Length operations"
  [ bench1
  , bench2
  , bench3
  ]
```

<!-- PD_END -->

Here we use the `group_` function which takes a string (the name of the group) and an array of benchmarks. The return type of `group_` is simply a `Group`.

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - tag: signature
    name: group_
-->

```purescript
group_ :: forall a b. String -> Array (Bench a b) -> Group
```

<!-- PD_END -->

## Options

In analogy to the `bench`/`bench_` functions, there is also a `group` function that allows to define further options for the group.

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - tag: signature
    name: group
-->

```purescript
group
  :: forall a b
   . String
  -> (GroupOpts a b -> GroupOpts a b)
  -> Array (Bench a b)
  -> Group
```

<!-- PD_END -->

Whereas the `GroupOpts` record is defined as follows:

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - GroupOpts
-->

```purescript
type GroupOpts a b =
  { sizes :: Array Int
  , iterations :: Int
  , check :: Maybe (Size -> Array (a /\ b) -> Boolean)
  , printInput :: Maybe (a -> String)
  , printOutput :: Maybe (b -> String)
  }
```

<!-- PD_END -->

- The `sizes` field specifies the different sizes to generate input data for the benchmarks. The benchmarks will be run for each size in the array.
- The `iterations` field specifies how many times each benchmark should be run for each size. This field can be overridden by the `iterations` field in the `BenchOpts` record.
- The `check` field is an optional function that checks whether the inputs and the outputs for each size are correct throughout all benchmarks in the group. It takes a size and an array of pairs of inputs and outputs, and returns a `Boolean`. If the function returns `false`, the benchmark will be marked as failed.
- The `printInput` and `printOutput` fields are optional functions that print the input and output of the benchmark for each size. This is used in error messages only.

## Normalization

# Suites

## Runners

## Reporters

### Console

### HTML

### JSON

# Full Examples

## Basic

<!-- PD_START:purs
filePath: test/Test/Doc.purs
pick:
  - fullExample1
-->

```purescript
fullExample1 :: Effect Unit
fullExample1 = BL.runNode_ $
  BL.suite_
    "Sample"
    [ BL.group_
        "Length operations"
        [ BL.bench_
            "Length of Array of Char"
            { prepare: \size -> Array.replicate size 'x'
            , run: Array.length
            }
        , BL.bench_
            "Length of List of Char"
            { prepare: \size -> List.replicate size 'x'
            , run: List.length
            }
        , BL.bench_
            "Length of String"
            { prepare: \size -> String.fromCharArray (Array.replicate size 'x')
            , run: String.length
            }
        ]
    ]
```

<!-- PD_END -->

# Effectful Benchmarks

# Appendix

## Module Imports

## Convenience Functions
