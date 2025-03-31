module Test.Samples.Simple where

import Prelude

import BenchLib (bench, benchGroup_, benchSuite, reportConsole)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml_)
import BenchLib.Reporters.Json (reportJson_)
import Data.List (List)
import Data.List as List
import Effect (Effect)

main :: Effect Unit
main = do
  let
    mkItems :: Int -> List Int
    mkItems n = List.range 1 n

  BenchLib.run $
    benchSuite
      "Simple Example"

      -- set suite options by overriding default config:
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
      [ benchGroup_ "List operations"
          [ bench
              "Add item to the front of a list"
              -- set benchmark options:
              ( \cfg -> cfg
                  { prepare = \size -> mkItems size -- runs before each benchmark
                  }
              )
              -- benchmark function:
              (\items -> List.Cons 0 items)

          , bench
              "Add item to the end of a list"
              -- set benchmark options:
              ( \cfg -> cfg
                  { prepare = \size -> mkItems size -- runs before each benchmark
                  }
              )
              -- benchmark function:
              (\items -> List.snoc items 0)
          ]
      ]

