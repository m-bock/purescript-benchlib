--- Header

module Test.Samples.HtmlReporter (main) where

import Prelude

import BenchLib (group_, suite, bench_)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml)
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)

--- Main

reporters :: Array BenchLib.Reporter
reporters =
  [ reportHtml \cfg -> cfg
      { filePath = "docs/outputs/report.html"
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