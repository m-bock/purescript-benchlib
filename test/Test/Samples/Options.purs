module Test.Samples.Options where

import Prelude

import BenchLib (bench, group, suite)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List as List
import Effect (Effect)

main :: Effect Unit
main = BenchLib.runNode_ $
  suite
    "Sample"
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
            { prepare: identity
            , run: \size -> Array.range 0 size
            }

        , bench
            "Create a range of numbers in a list"
            ( \cfg -> cfg
                { iterations = 1000
                }
            )
            { prepare: identity
            , run: \size -> List.range 0 size
            }
        ]
    ]

