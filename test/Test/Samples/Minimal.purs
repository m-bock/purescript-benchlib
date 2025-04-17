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

