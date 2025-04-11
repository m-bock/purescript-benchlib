# purescript-benchlib

![logo](docs/logo.png)

A simple and flexible benchmarking library for PureScript, designed to help you measure performance with ease. 

## Features

- __Customizable Output__ – Choose between console logging, JSON reports, HTML or Markdown summaries with rendered charts.

- __Flexible Benchmarking__ – Configure iterations, input sizes, and setup functions.

- __Supports Pure & Effectful Functions__ – Benchmark any kind of computation.

- __Pure PureScript__ – No JavaScript libraries under the hood, ensuring full type safety.

<br>

Focus on writing benchmarks, not boilerplate. 

## Limitations

- Currently, the library only supports Node.js. However, we plan to add browser support in the future.

- The library is still in its early stages, and we welcome contributions to improve it.


## Example to Get You Started

### 1 . Define Benchmarks in PureScript

```purescript
module Test.Samples.Minimal where

import Prelude

import BenchLib (basic, group_, suite_, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)

main :: Effect Unit
main = BenchLib.run_ $
  suite_
    "Minimal Example"
    [ group_ "Replicate Functions"
        [ basic $ bench_
            "Array"
            (\size -> Array.replicate size 'x')

        , basic $ bench_
            "Lazy List"
            (\size -> LazyList.replicate size 'x')
        ]
    ]

```

### 2. Run Benchmarks from Terminal

After defining your benchmarks, you can run them from the terminal using `spago`:

```bash
spago run -m Test.Samples.Minimal
```
