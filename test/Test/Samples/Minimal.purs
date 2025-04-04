--- Header

module Test.Samples.Minimal where

import Prelude
import BenchLib (benchGroup_, benchSuite_, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)

--- Main

main :: Effect Unit
main = BenchLib.run $
  benchSuite_
    "Minimal Example"
    [ benchGroup_ "Replicate Functions"
        [ bench_
            "Array"
            (\size -> const unit $ Array.replicate size 'x')

        , bench_
            "Lazy List"
            (\size -> const unit $ LazyList.replicate size 'x')
        ]
    ]

