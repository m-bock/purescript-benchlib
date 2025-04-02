module Test.Samples.SuiteOptions where

import Prelude

import BenchLib (benchGroup_, benchSuite, bench_, reportConsole)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml_)
import BenchLib.Reporters.Json (reportJson_)
import Data.Array as Array
import Data.List as List
import Effect (Effect)

main :: Effect Unit
main = BenchLib.run $
  benchSuite
    "Minimal Example"
    ( \cfg -> cfg
        { iterations = 1000 -- number of iterations each benchmark will run
        , sizes = [ 20_000, 40_000, 80_000 ] -- input sizes for prepare functions

        -- different reporters depending on your needs:
        , reporters =
            [ reportConsole -- Simply logs benchmarks to the console
            , reportJson_ -- Writes benchmarks to JSON file
            , reportHtml_ -- Writes benchmarks to HTML file
            ]
        }
    )
    [ benchGroup_ "range functions"
        [ bench_
            "Create a range of numbers in an array"
            (\size -> Array.range 0 size)

        , bench_
            "Create a range of numbers in a list"
            (\size -> List.range 0 size)
        ]
    ]

