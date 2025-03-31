# purescript-benchlib

![logo](docs/logo.svg)

## Minimal Example
<details>
  <summary>Expand to see module imports</summary>

```purescript
module Test.Samples.Simple where

import Prelude
import BenchLib (benchSuite_, benchGroup_, bench_)
import BenchLib as BenchLib
import Data.List (List)
import Data.List as List
import Effect (Effect)
```

</details>


```purescript
main :: Effect Unit
main = do
  let
    items :: List Int
    items = List.range 1 100_000

  BenchLib.run $
    benchSuite_ "Simple Example"
      [ benchGroup_ "List operations"
          [ bench_ "Add item to the front of a list"
              (\_ -> List.Cons 0 items)

          , bench_ "Add item to the end of a list"
              (\_ -> List.snoc items 0)
          ]
      ]
```

Run the benchmarks in a terminal

```bash
spago run -m Test.Samples.Simple
```

The result will look like:

```bash
• suite: Simple Example
  • group: List operations
    • size: 0
      • bench: Add item to the front of a list (size = 0)
        • mean duration: 0.001 ms (1000 iterations)

      • bench: Add item to the end of a list (size = 0)
        • mean duration: 19.4 ms (1000 iterations)

    • size: 10
      • bench: Add item to the front of a list (size = 10)
        • mean duration: 0.001 ms (1000 iterations)

      • bench: Add item to the end of a list (size = 10)
        • mean duration: 19.266 ms (1000 iterations)

    • size: 100
      • bench: Add item to the front of a list (size = 100)
        • mean duration: 0.0 ms (1000 iterations)

      • bench: Add item to the end of a list (size = 100)
        • mean duration: 19.32 ms (1000 iterations)
```