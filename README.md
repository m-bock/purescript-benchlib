# purescript-benchlib

![logo](docs/logo.svg)

## Minimal Example
<details>
  <summary>Expand to see module header and imports</summary>

```purescript
module Test.Samples.Simple where

import Prelude

import BenchLib (bench, benchGroup_, benchSuite, reportConsole)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml_)
import BenchLib.Reporters.Json (reportJson_)
import Data.List as List
import Effect (Effect)
```

</details>


```purescript
main :: Effect Unit
main = BenchLib.run $
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
                { prepare = \size -> List.range 1 size -- runs before each benchmark
                }
            )
            -- benchmark function:
            (\items -> List.Cons 0 items)

        , bench
            "Add item to the end of a list"
            -- set benchmark options:
            ( \cfg -> cfg
                { prepare = \size -> List.range 1 size -- runs before each benchmark
                }
            )
            -- benchmark function:
            (\items -> List.snoc items 0)
        ]
    ]
```

Run the benchmarks in a terminal

```bash
spago run -m Test.Samples.Simple
```

The result console output will look like:

```bash
• suite: Simple Example
  • group: List operations
    • size: 20000
      • bench: Add item to the front of a list (size = 20000)
        • mean duration: 0.0 ms (1000 iterations)

      • bench: Add item to the end of a list (size = 20000)
        • mean duration: 1.833 ms (1000 iterations)

    • size: 40000
      • bench: Add item to the front of a list (size = 40000)
        • mean duration: 0.005 ms (1000 iterations)

      • bench: Add item to the end of a list (size = 40000)
        • mean duration: 4.731 ms (1000 iterations)

    • size: 80000
      • bench: Add item to the front of a list (size = 80000)
        • mean duration: 0.001 ms (1000 iterations)

      • bench: Add item to the end of a list (size = 80000)
        • mean duration: 14.255 ms (1000 iterations)

Wrote JSON report to bench-results.json
Wrote HTML report to bench-results.html
```

The HTML report in `bench-results.html` will look like:

![ChartJs output](docs/screenshot-simple-results.png)

It clearly demonstrates that List.Cons operates in constant time (O(1)), whereas List.snoc requires linear time (O(n)).