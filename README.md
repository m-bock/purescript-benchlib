# purescript-benchlib

<img src="docs/logo.png" />

A simple and flexible benchmarking library for PureScript.

## Features

- **Customizable Output** – Choose between console logging, JSON reports, HTML or Markdown summaries with rendered charts.

- **Flexible Benchmarking** – Configure iterations, input sizes, and setup functions.

- **Supports Pure & Effectful Functions** – Benchmark any kind of computation.

- **Just PureScript** – No JavaScript libraries under the hood, ensuring full type safety.

<br>

Focus on writing benchmarks, not boilerplate.

## Documentation

- [API Documentation](https://pursuit.purescript.org/packages/purescript-benchlib)
- [Tutorial](docs/tutorial.md)

## Limitations

- Currently, the library only supports Node.js. However, it's planned to add browser support in the future.

- The library is still in its early stages, and we welcome contributions to improve it.

## Example to Get You Started

### 1 . Define Benchmarks in PureScript

```purescript
--- Header

module Test.Samples.Minimal where

import Prelude

import BenchLib (group_, suite_, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)

--- Main

main :: Effect Unit
main = BenchLib.runNode_ $
  suite_
    "Minimal Example"
    [ group_ "Replicate Functions"
        [ bench_
            "Array"
            { prepare: identity
            , run: \size -> Array.replicate size 'x'
            }
        , bench_
            "Lazy List"
            { prepare: identity
            , run: \size -> LazyList.replicate size 'x'
            }
        ]
    ]
```

### 2. Run Benchmarks from Terminal

After defining your benchmarks, you can run them from the terminal using `spago`:

```bash
spago run -m Test.Samples.Minimal
```
