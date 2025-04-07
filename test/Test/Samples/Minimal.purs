--- Header

module Test.Samples.Minimal where

import Prelude

import BenchLib (basic, benchGroup_, benchSuite_, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)

--- Main

main :: Effect Unit
main = BenchLib.run_ $
  benchSuite_
    "Minimal Example"
    [ benchGroup_ "Replicate Functions"
        [ basic $ bench_
            "Array"
            (\size -> Array.replicate size 'x')

        , basic $ bench_
            "Lazy List"
            (\size -> LazyList.replicate size 'x')
        ]
    ]

