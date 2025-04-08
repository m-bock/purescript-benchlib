# purescript-benchlib

![logo](docs/logo.png)

A simple and flexible benchmarking library for PureScript, designed to help you measure performance with ease. 

## Features

- __Customizable Output__ – Choose between console logging, JSON reports, or HTML summaries with rendered charts.

- __Flexible Benchmarking__ – Configure iterations, input sizes, and setup functions.

- __Supports Pure & Effectful Functions__ – Benchmark any kind of computation.

- __Pure PureScript__ – No JavaScript libraries under the hood, ensuring full type safety.

<br>

Focus on writing benchmarks, not boilerplate. 

## Limitations

- Currently, the library only supports Node.js. However, we plan to add browser support in the future.

- The library is still in its early stages, and we welcome contributions to improve it.


## Example to Get You Started

### 1 . Define Benchmarks in PureScript
<details>
  <summary>Expand to see module header and imports</summary>

```purescript
module Test.Samples.Simple where

import Prelude

import BenchLib (bench, group_, suite, reportConsole)
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
  suite
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
    [ group_ "List operations"
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

### 2. Run the benchmarks from terminal

After defining your benchmarks, you can run them from the terminal using `spago`:

```bash
spago run -m Test.Samples.Simple
```

### 3. Results

#### Console Output

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

####  HTML Output

The HTML report in `bench-results.html` will look like:

![ChartJs output](docs/screenshot-simple-results.png)

It clearly demonstrates that List.Cons operates in constant time (O(1)), whereas List.snoc requires linear time (O(n)).

#### JSON Output

If you need custom postprocessing, you can use the JSON report in `bench-results.json`.