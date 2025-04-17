--- Header

module Test.Samples.JsonReporter (main) where

import Prelude

import BenchLib (group_, suite, bench_)
import BenchLib as BenchLib
import BenchLib.Reporters.Json (reportJson)
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)

--- Main

reporters :: Array BenchLib.Reporter
reporters =
  [ reportJson \cfg -> cfg
      { filePath = "docs/outputs/report.json"
      , indent = 2
      }
  ]

main :: Effect Unit
main =
  BenchLib.runNode
    ( \cfg -> cfg
        { reporters = cfg.reporters <> reporters }
    )
    $ suite
        "Sample"
        ( \cfg -> cfg
            { iterations = 1000
            , sizes = [ 0, 20_000, 40_000, 80_000 ]
            }
        )
        [ group_
            "Replicate functions"
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