<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Benchmarks](#benchmarks)
  - [Type Signatures](#type-signatures)
  - [Preparation Functions](#preparation-functions)
  - [Benchmark Definitions](#benchmark-definitions)
  - [Options](#options)
- [Groups](#groups)
  - [Basic](#basic)
  - [Normalized](#normalized)
  - [Options](#options-1)
- [Suites](#suites)
- [Runners](#runners)
  - [Node](#node)
- [Reporters](#reporters)
  - [Console](#console)
  - [HTML](#html)
  - [JSON](#json)
- [Effectful Benchmarks](#effectful-benchmarks)
- [Appendix](#appendix)
  - [Module Imports](#module-imports)
  - [Convenience Functions](#convenience-functions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Benchmarks

## Type Signatures

## Preparation Functions

## Benchmark Definitions

## Options

# Groups

## Basic

## Normalized

## Options

# Suites

## Runners

## Reporters

### Console

### HTML

### JSON

# Effectful Benchmarks

# Appendix

## Module Imports

## Convenience Functions

---

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

<!-- PD_START:purs
filePath: src/BenchLib.purs
inline: true
pick:
  - tag: signature
    name: bench_
-->`bench_ :: forall a b. String -> BenchBaseOpts a b -> Bench Unit Unit`<!-- PD_END -->

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

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - tag: signature
    name: group_
-->
```purescript
-- | Like `group`, but with default options.
group_ :: forall a b. String -> Array (Bench a b) -> Group
```
<!-- PD_END -->

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

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - BenchOpts
-->
```purescript
-- | Options for pure benchmarks.
type BenchOpts a' b' a b =
  { iterations :: Int
  , normIn :: a -> a'
  , normOut :: b -> b'
  }
```
<!-- PD_END -->

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - BenchOpts
-->
```purescript
-- | Options for pure benchmarks.
type BenchOpts a' b' a b =
  { iterations :: Int
  , normIn :: a -> a'
  , normOut :: b -> b'
  }
```
<!-- PD_END -->

<!-- PD_START:purs
filePath: src/BenchLib.purs
pick:
  - GroupOpts
-->
```purescript
-- | Options for the benchmark group.
type GroupOpts a b =
  { sizes :: Array Int
  , iterations :: Int
  , check :: Maybe (Size -> Array (a /\ b) -> Boolean)
  , printInput :: Maybe (a -> String)
  , printOutput :: Maybe (b -> String)
  }
```
<!-- PD_END -->
