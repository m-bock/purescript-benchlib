<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Benchmarks](#benchmarks)
  - [Type Signatures](#type-signatures)
  - [Preparation Functions](#preparation-functions)
  - [Benchmark Definitions](#benchmark-definitions)
  - [Options](#options)
- [Groups](#groups)
  - [Basic](#basic)
  - [Options](#options-1)
  - [Normalization](#normalization)
- [Suites](#suites)
  - [Runners](#runners)
  - [Reporters](#reporters)
    - [Console](#console)
    - [HTML](#html)
    - [JSON](#json)
- [Full Examples](#full-examples)
  - [Basic](#basic-1)
- [Effectful Benchmarks](#effectful-benchmarks)
- [Appendix](#appendix)
  - [Module Imports](#module-imports)
  - [Convenience Functions](#convenience-functions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Benchmarks

In this tutorial, we'll use **benchlib** to compare the runtime performance of computing the length of three different PureScript collections: `Array`, `List`, and `String`.

## Type Signatures

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

These functions will be fed with various sizes to prepare the input data for our benchmarks.

## Benchmark Definitions

<!-- PD_START:purs
filePath: src/BenchLib.purs
inline: true
pick:
  - tag: signature
    name: bench_
-->
`bench_ :: forall a b. String -> BenchBaseOpts a b -> Bench Unit Unit`
<!-- PD_END -->

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

## Options

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

# Groups

## Basic

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

## Options

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
