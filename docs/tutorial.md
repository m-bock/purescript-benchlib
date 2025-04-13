# benchlib Tutorial

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pure Benchmarks](#pure-benchmarks)
  - [Type Signatures](#type-signatures)
  - [Preparation Functions](#preparation-functions)
  - [Benchmark Definitions](#benchmark-definitions)
- [Groups](#groups)
  - [Basic](#basic)
  - [Normalized](#normalized)
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

---

## Pure Benchmarks

In this tutorial, we'll use **benchlib** to compare the runtime performance of computing the length of three different PureScript collections: `Array`, `List`, and `String`.

### Type Signatures

Here are the type signatures of the functions we'll benchmark:

- `Array.length :: forall a. Array a -> Int`
- `List.length :: forall a. List a -> Int`
- `String.length :: String -> Int`

<!-- start:raw
name: Foo
bar: true
baz: 42
--> <!-- end -->

Each function computes the number of elements in its respective collection type.

---
### Preparation Functions

Before we define our actual benchmarks, we need to create functions that prepare input data of a given size.

```purescript
type Size = Int

prepareCharArray :: Size -> Array Char
prepareCharArray size = Array.replicate size 'x'

prepareCharList :: Size -> List Char
prepareCharList size = List.replicate size 'x'

prepareString :: Size -> String
prepareString size = String.fromCharArray (Array.replicate size 'x')
```

Each function takes a `Size` (an alias for `Int`) and returns a uniformly filled collection of that size using the character `'x'`.

We will benchmark these functions at varying sizes to analyze the performance trends.

---

### Benchmark Definitions

```purescript
b1 :: Bench (Array Char) Int
b1 = BL.bench "Length of Array of Char" (\opts ->
  opts {
    iterations = 1000,
    prepareInput = prepareCharArray
  })
  Array.length

```

```purescript
b2 :: Bench (List Char) Int
b2 = BL.bench "Length of List of Char" (\opts ->
  opts {
    iterations = 1000,
    prepareInput = prepareCharList
  })
  List.length

```

```purescript
b3 :: Bench String Int
b3 = BL.bench "Length of String" (\opts ->
  opts {
    iterations = 1000,
    prepareInput = prepareString
  })
  String.length

```

---


---

## Groups



### Basic

(*Explain how to organize related benchmarks into basic groups for comparison.*)

### Normalized

(*Show how to normalize benchmark results, e.g., by comparing against a baseline.*)

---

## Suites

(*Describe how to combine multiple groups into a test suite to be run together.*)

---

## Runners

### Node

(*Show how to run the suite in a Node.js environment, using CLI commands or build tools.*)

---

## Reporters

### Console

(*Output benchmark results to the terminal.*)

### HTML

(*Generate interactive visual reports in HTML format.*)

### JSON

(*Export benchmark data for programmatic use or further analysis.*)

---

## Effectful Benchmarks

(*Demonstrate how to benchmark side-effecting or asynchronous computations.*)

---

## Appendix

### Module Imports

(*List the necessary imports for each section above — e.g., `Benchmark`, `Group`, `Suite`, etc.*)

### Convenience Functions

(*Highlight helper functions that simplify writing and organizing benchmarks.*)