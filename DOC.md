```hs
module Test.Samples.Minimal where

import Prelude

import BenchLib (benchGroup_, benchSuite, benchSuite_, bench_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List as List
import Data.List.Lazy (replicate)
import Data.List.Lazy as LazyList
import Effect (Effect)


main :: Effect Unit
main = BenchLib.run $
  benchSuite_
    "Minimal Example"
    [ benchGroup_ "range functions"
        [ bench_
            "Create an array of given size"
            (\size -> Array.replicate size 'x')

        , bench_
            "Create a lazy list of given size"
            (\size -> LazyList.replicate size 'x')
        ]
    ]
```

RUN: node -e 'import("./output/Test.Samples.Minimal/index.js").then(m => m.main())'


```hs

```

<iframe width="100%" height="600" src="""></iframe>


```hs
main2 :: Effect Unit
main2 = BenchLib.run $
  benchSuite
    "Minimal Example"
    ( \cfg -> cfg
        { iterations = 10000
        , sizes = [ 20_000, 40_000, 80_000 ]
        , reporters = []
        }
    )
    [ benchGroup_ "range functions"
        [ bench_
            "Create an array of given size"
            (\size -> Array.replicate size 'x')

        , bench_
            "Create a lazy list of given size"
            (\size -> LazyList.replicate size 'x')
        ]
    ]
```