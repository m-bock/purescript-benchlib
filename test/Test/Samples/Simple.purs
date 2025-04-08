module Test.Samples.Simple where

import Prelude

import BenchLib (basic, bench, group_, suite, reportConsole)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml_)
import BenchLib.Reporters.Json (reportJson_)
import Data.Array as Array
import Data.List as List
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
    suite
      "Simple Example"
      -- set suite options by overriding default config:
      ( \cfg -> cfg
          { iterations = 1000 -- number of iterations each benchmark will run
          , sizes = [ 20_000, 40_000, 80_000 ] -- input sizes for prepare functions
          }
      )
      [ group_
          "List operations"
          [ basic $ bench
              "Add item to the end of a list"
              -- set benchmark options:
              ( \cfg -> cfg
                  { prepareInput = \size -> List.range 1 size -- runs before each benchmark
                  }
              )
              -- benchmark function:
              (\items -> List.snoc items 0)

          , basic $ bench
              "Add item to the start of a list"
              -- set benchmark options:
              ( \cfg -> cfg
                  { prepareInput = \size -> List.range 1 size -- runs before each benchmark
                  }
              )
              -- benchmark function:
              (\items -> List.Cons 0 items)

          , basic $ bench
              "Add item to the start of an array"
              -- set benchmark options:
              ( \cfg -> cfg
                  { prepareInput = \size -> Array.range 1 size -- runs before each benchmark
                  }
              )
              -- benchmark function:
              (\items -> Array.cons 0 items)
          ]
      ]

