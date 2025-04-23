# benchlib Tutorial

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

---

## Benchmarks

In this tutorial, we'll use **benchlib** to compare the runtime performance of computing the length of three different PureScript collections: `Array`, `List`, and `String`.

### Type Signatures

Here are the type signatures of the functions we'll benchmark:

<!-- PATCH_START pursSig
filePath: .spago/p/arrays-7.3.0/src/Data/Array.purs
ident: length
moduleAlias: Array
prefix: '- '
 -->- `Array.length :: forall a. Array a -> Int`<!-- END -->
<!-- PATCH_START pursSig
filePath: .spago/p/lists-7.0.0/src/Data/List.purs
ident: length
moduleAlias: List
prefix: '- '
 -->- `List.length :: forall a. List a -> Int`<!-- END -->
<!-- PATCH_START pursSig
filePath: .spago/p/strings-6.0.1/src/Data/String/CodePoints.purs
ident: length
moduleAlias: Array
prefix: '- '
 -->- `Array.length :: String -> Int`<!-- END -->

Each function computes the number of elements in its respective collection type.

---

### Preparation Functions

Before we define our actual benchmarks, we need to create functions that prepare input data of a given size.

<!-- PATCH_START pursType
filePath: src/BenchLib.purs
ident: Size
 -->

```purescript
type Size = Int
```

<!-- END -->

<!-- PATCH_START pursVal
filePath: test/Test/Doc.purs
idents:
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

<!-- END -->

Each function takes a `Size` (an alias for `Int`) and returns a uniformly filled collection of that size using the character `'x'`.

We will benchmark these functions at varying sizes to analyze the performance trends.

---

### Benchmark Definitions

<!-- PATCH_START pursSig
filePath: src/BenchLib.purs
ident: bench_
 -->`bench_ :: forall a b. String -> (Size -> a) -> (a -> b) -> Bench a b`<!-- END -->

<!-- PATCH_START pursVal
filePath: test/Test/Doc.purs
idents:
  - bench1
  - bench2
  - bench3
 -->

```purescript
bench1 :: Bench (Array Char) Int
bench1 = BL.bench_ "Length of Array of Char"
  prepareCharArray
  Array.length

bench2 :: Bench (List Char) Int
bench2 = BL.bench_ "Length of List of Char"
  prepareCharList
  List.length

bench3 :: Bench String Int
bench3 = BL.bench_ "Length of String"
  prepareString
  String.length
```

<!-- END -->

### Options

<!-- PATCH_START pursSig
filePath: src/BenchLib.purs
ident: bench
 -->`bench :: forall a b. String -> (BenchOpts -> BenchOpts) -> (Size -> a) -> (a -> b) -> Bench a b`<!-- END -->

<!-- PATCH_START pursType
filePath: src/BenchLib.purs
ident: BenchOpts
 -->

```purescript
type BenchOpts =
  { iterations :: Int
  }
```

<!-- END -->

---

## Groups

<!-- PATCH_START pursSig
filePath: test/Test/Doc.purs
ident: bench1
prefix: '- '
 -->- `bench1 :: Bench (Array Char) Int`<!-- END -->
<!-- PATCH_START pursSig
filePath: test/Test/Doc.purs
ident: bench2
prefix: '- '
 -->- `bench2 :: Bench (List Char) Int`<!-- END -->
<!-- PATCH_START pursSig
filePath: test/Test/Doc.purs
ident: bench3
prefix: '- '
 -->- `bench3 :: Bench String Int`<!-- END -->

### Basic

<!-- PATCH_START pursSig
filePath: src/BenchLib.purs
ident: group_
 -->`group_ :: forall a b. String -> Array (Bench a b) -> Group`<!-- END -->

<!-- PATCH_START pursVal
filePath: test/Test/Doc.purs
idents:
  - benchBasic1
  - benchBasic2
  - benchBasic3
 -->

```purescript
benchBasic1 :: Bench Unit Unit
benchBasic1 = BL.basic bench1

benchBasic2 :: Bench Unit Unit
benchBasic2 = BL.basic bench2

benchBasic3 :: Bench Unit Unit
benchBasic3 = BL.basic bench3
```

<!-- END -->

<!-- PATCH_START pursVal
filePath: test/Test/Doc.purs
idents:
  - groupBasic
 -->

```purescript
groupBasic :: Group
groupBasic = BL.group_
  "Length operations"
  [ benchBasic1
  , benchBasic2
  , benchBasic3
  ]
```

<!-- END -->

(_Explain how to organize related benchmarks into basic groups for comparison._)

### Normalized

<!-- PATCH_START pursVal
filePath: test/Test/Doc.purs
idents:
  - benchNormalized1
  - benchNormalized2
  - benchNormalized3
 -->

```purescript
benchNormalized1 :: Bench (Array Char) Int
benchNormalized1 = bench1

benchNormalized2 :: Bench (Array Char) Int
benchNormalized2 = BL.normalizeInput List.toUnfoldable bench2

benchNormalized3 :: Bench (Array Char) Int
benchNormalized3 = BL.normalizeInput String.toCharArray bench3
```

<!-- END -->

<!-- PATCH_START pursVal
filePath: test/Test/Doc.purs
idents:
  - groupNormalized
 -->

```purescript
groupNormalized :: Group
groupNormalized = BL.group_
  "Length of Char"
  [ benchNormalized1
  , benchNormalized2
  , benchNormalized3
  ]
```

<!-- END -->

(_Show how to normalize benchmark results, e.g., by comparing against a baseline._)

### Options

<!-- PATCH_START pursSig
filePath: src/BenchLib.purs
ident: group
 -->`group :: forall a b

. String
-> (GroupOpts a b -> GroupOpts a b)
-> Array (Bench a b)
-> Group`<!-- END -->

<!-- END -->

<!-- PATCH_START pursRecord
filePath: src/BenchLib.purs
ident: GroupOpts
descriptions:
  sizes: List of sizes to test
  iterations: Number of iterations
  checkInputs: Optional input validator
  checkOutputs: Optional output validator
  printInput: Optional input printer
  printOutput: Optional output printer
 -->

#### `type GroupOpts a b = {...}`

<table style='width:100%'>
  <tr>
    <th align='left'>Field</th>
    <th align='left'>Type</th>
    <th align='left'>Description</th>
  </tr>
  <tr>
    <td valign='top'>
      <code>sizes</code>
    </td>
    <td valign='top'>
      <code>Array Int</code>
    </td>
    <td valign='top'>List of sizes to test</td>
  </tr>
  <tr>
    <td valign='top'>
      <code>iterations</code>
    </td>
    <td valign='top'>
      <code>Int</code>
    </td>
    <td valign='top'>Number of iterations</td>
  </tr>
  <tr>
    <td valign='top'>
      <code>checkInputs</code>
    </td>
    <td valign='top'>
      <code>Maybe (Size -> Array a -> Boolean)</code>
    </td>
    <td valign='top'>Optional input validator</td>
  </tr>
  <tr>
    <td valign='top'>
      <code>checkOutputs</code>
    </td>
    <td valign='top'>
      <code>Maybe (Size -> Array b -> Boolean)</code>
    </td>
    <td valign='top'>Optional output validator</td>
  </tr>
  <tr>
    <td valign='top'>
      <code>printInput</code>
    </td>
    <td valign='top'>
      <code>Maybe (a -> String)</code>
    </td>
    <td valign='top'>Optional input printer</td>
  </tr>
  <tr>
    <td valign='top'>
      <code>printOutput</code>
    </td>
    <td valign='top'>
      <code>Maybe (b -> String)</code>
    </td>
    <td valign='top'>Optional output printer</td>
  </tr>
</table><!-- END -->

---

## Suites

(_Describe how to combine multiple groups into a test suite to be run together._)

---

## Runners

### Node

(_Show how to run the suite in a Node.js environment, using CLI commands or build tools._)

---

## Reporters

### Console

(_Output benchmark results to the terminal._)

### HTML

(_Generate interactive visual reports in HTML format._)

### JSON

(_Export benchmark data for programmatic use or further analysis._)

---

## Effectful Benchmarks

(_Demonstrate how to benchmark side-effecting or asynchronous computations._)

---

## Appendix

### Module Imports

(_List the necessary imports for each section above — e.g., `Benchmark`, `Group`, `Suite`, etc._)

### Convenience Functions

(_Highlight helper functions that simplify writing and organizing benchmarks._)
