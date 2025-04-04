# The benchlib guide

## Minimal example

### Define the benchmark in PureScript

<!-- start:code
{ 
  "file": "test/Test/Samples/Minimal.purs",
  "section": "Header",
  "collapsible": true,
  "link": true
}
-->
[Source Code](test/Test/Samples/Minimal.purs)
<details>
<summary>Show Module header and imports</summary>

```purescript
module Test.Samples.Minimal where

import Prelude

import BenchLib (benchGroup_, benchSuite_, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)
```

</details>
<!-- end -->

<!-- start:code
{"file": "test/Test/Samples/Minimal.purs", "section": "Main"}
-->

```purescript
main :: Effect Unit
main = BenchLib.run $
  benchSuite_
    "Minimal Example"
    [ benchGroup_ "range functions"
        [ bench_
            "Array"
            (\size -> Array.replicate size 'x')

        , bench_
            "Lazy List"
            (\size -> LazyList.replicate size 'x')
        ]
    ]
```
<!-- end -->


### Run the benchmark from Command Line

<!-- start:run
{"cmd": "npx spago run --main Test.Samples.Minimal"}
-->
```bash
npx spago run --main Test.Samples.Minimal
```

```text
• suite: Minimal Example
  • group: range functions
    • bench: Array
      • size=0, duration=0.0060ms, iterations=1000
      • size=25000, duration=0.1590ms, iterations=1000
      • size=50000, duration=0.2880ms, iterations=1000
      • size=100000, duration=0.5050ms, iterations=1000
    • bench: Lazy List
      • size=0, duration=0.0020ms, iterations=1000
      • size=25000, duration=0.0030ms, iterations=1000
      • size=50000, duration=0.0030ms, iterations=1000
      • size=100000, duration=0.0010ms, iterations=1000
```
<!-- end -->