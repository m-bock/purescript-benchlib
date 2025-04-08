module Test.Samples.Options where

import Prelude

import BenchLib (bench, group, group_, suite, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List as List
import Effect (Effect)

main :: Effect Unit
main = BenchLib.run_ $
  suite
    "Minimal Example"
    ( \cfg -> cfg
        { iterations = 1000
        , sizes = [ 20_000, 40_000, 80_000 ]
        }
    )
    [ group
        "range functions"
        ( \cfg -> cfg
            { iterations = 1000
            , sizes = [ 20_000, 40_000, 80_000 ]
            }
        )
        [ bench
            "Create a range of numbers in an array"
            ( \cfg -> cfg
                { iterations = 1000
                }
            )
            (\size -> const unit $ Array.range 0 size)

        , bench
            "Create a range of numbers in a list"
            ( \cfg -> cfg
                { iterations = 1000
                }
            )
            (\size -> const unit $ List.range 0 size)
        ]
    ]

