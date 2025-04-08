module Test.Samples.PrepareFunction where

import Prelude

import BenchLib (basic, bench, group_, reportConsole, suite, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml_)
import BenchLib.Reporters.Json (reportJson_)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Unfoldable (replicate)
import Effect (Effect)

main :: Effect Unit
main =
  BenchLib.run
    ( \cfg -> cfg
        { reporters =
            [ reportConsole -- Simply logs benchmarks to the console
            , reportJson_ -- Writes benchmarks to JSON file
            , reportHtml_ -- Writes benchmarks to HTML file
            ]
        }
    ) $
    suite_
      "Simple Example"

      [ group_
          "Get last element"
          [ basic $ bench
              "List"
              ( \cfg -> cfg
                  { prepareInput = \(size :: Int) -> replicate size 'x'
                  }
              )
              (\(items :: List Char) -> List.last items)

          , basic $ bench
              "Array"
              ( \cfg -> cfg
                  { prepareInput = \(size :: Int) -> replicate size 'x'
                  }
              )
              (\(items :: Array Char) -> Array.last items)
          ]
      ]

