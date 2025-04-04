--- Header

module Test.Samples.MarkdownReporter (main) where

import Prelude
import BenchLib (benchGroup_, benchSuite, bench_)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml)
import BenchLib.Reporters.Json (reportJson)
import BenchLib.Reporters.Markdown (reportMarkdown)
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)

--- Reporters

reporters :: Array BenchLib.Reporter
reporters =
  [ reportMarkdown \cfg -> cfg
      { filePath = "docs/chapters/02_reporters/report.md"
      , showHeadline = true
      }

  , reportJson \cfg -> cfg
      { filePath = "docs/chapters/02_reporters/report.json"
      , indent = 2
      }

  , reportHtml \cfg -> cfg
      { filePath = "docs/chapters/02_reporters/report.html"
      , lineStyles =
          [ { color: { r: 255, g: 99, b: 132 }, opacity: 0.5, width: 2.0 }
          , { color: { r: 54, g: 162, b: 235 }, opacity: 0.5, width: 2.0 }
          , { color: { r: 153, g: 102, b: 255 }, opacity: 0.5, width: 2.0 }
          ]
      }
  ]

--- Main

main :: Effect Unit
main = BenchLib.run $
  benchSuite
    "Minimal Example"
    ( \cfg -> cfg
        { iterations = 1000
        , sizes = [ 0, 20_000, 40_000, 80_000 ]
        , reporters = cfg.reporters <> reporters
        }
    )
    [ benchGroup_ "Replicate functions"
        [ bench_
            "Array"
            (\size -> Array.replicate size 'x')

        , bench_
            "Lazy List"
            (\size -> LazyList.replicate size 'x')
        ]
    ]