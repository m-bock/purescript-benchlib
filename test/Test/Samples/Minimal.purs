module Test.Samples.Minimal where

import Prelude

import BenchLib (benchGroup_, benchSuite_, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List as List
import Effect (Effect)

main :: Effect Unit
main = BenchLib.run $
  benchSuite_
    "Minimal Example"
    [ benchGroup_ "range functions"
        [ bench_
            "Create a range of numbers in an array"
            (\size -> Array.range 0 size)

        , bench_
            "Create a range of numbers in a list"
            (\size -> List.range 0 size)
        ]
    ]

