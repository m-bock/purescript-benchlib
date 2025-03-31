# purescript-benchlib

![logo](docs/logo.svg)

## Minimal Example

```purescript
module Test.Samples.Simple where

import Prelude

import BenchLib (benchSuite_, benchGroup_, bench_)
import BenchLib as BenchLib
import Data.List (List)
import Data.List as List
import Data.Unfoldable (range)
import Effect (Effect)

main :: Effect Unit
main = do
  let
    items :: List Int
    items = range 1 100_000

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