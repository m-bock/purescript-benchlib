# JSON Reporter

## Configure Reporters

### Define the benchmark in PureScript

<!-- start:pursCode
{ 
  "file": "test/Test/Samples/MarkdownReporter.purs",
  "section": "Header",
  "collapsible": true,
  "link": true
}
-->
Source Code: [test/Test/Samples/MarkdownReporter.purs](test/Test/Samples/MarkdownReporter.purs)
<details>
<summary>Show/Hide imports</summary>

> ```purescript
> module Test.Samples.MarkdownReporter (main) where
> 
> import Prelude
> 
> import BenchLib (basic, group_, suite, bench_)
> import BenchLib as BenchLib
> import BenchLib.Reporters.Markdown (reportMarkdown)
> import Data.Array as Array
> import Data.List.Lazy as LazyList
> import Effect (Effect)
> ```

</details>
<!-- end -->

<!-- start:pursCode
{"file": "test/Test/Samples/MarkdownReporter.purs", "section": "Main"}
-->

> ```purescript
> reporters :: Array BenchLib.Reporter
> reporters =
>   [ reportMarkdown \cfg -> cfg
>       { filePath = "docs/chapters/02_reporters/report.md"
>       , showHeadline = true
>       }
>   ]
> 
> main :: Effect Unit
> main =
>   BenchLib.run
>     ( \cfg -> cfg
>         { reporters = cfg.reporters <> reporters }
>     )
>     $ suite
>         "Minimal Example"
>         ( \cfg -> cfg
>             { iterations = 1000
>             , sizes = [ 0, 20_000, 40_000, 80_000 ]
>             }
>         )
>         [ group_ "Replicate functions"
>             [ basic $ bench_
>                 "Array"
>                 (\size -> Array.replicate size 'x')
> 
>             , basic $ bench_
>                 "Lazy List"
>                 (\size -> LazyList.replicate size 'x')
>             ]
>         ]
> ```
<!-- end -->

### Run the benchmark from Command Line

<!-- start:run
{"cmd": "npx spago run --main Test.Samples.JsonReporter", "hide": true}
-->
```bash
npx spago run --main Test.Samples.JsonReporter
```


<!-- end -->

### View the report


<!-- start:code 
{
  "file": "docs/chapters/02_reporters/report.json",
  "link": true,
  "language": "json"
}
-->
Source Code: [docs/chapters/02_reporters/report.json](docs/chapters/02_reporters/report.json)
> ```json
> {
>   "groupResults": [
>     {
>       "benchResults": [
>         {
>           "benchName": "Array",
>           "samples": [
>             {
>               "average": 0.00253740700000003,
>               "iterations": 1000,
>               "size": 0
>             },
>             {
>               "average": 0.143320605,
>               "iterations": 1000,
>               "size": 20000
>             },
>             {
>               "average": 0.32965885800000005,
>               "iterations": 1000,
>               "size": 40000
>             },
>             {
>               "average": 0.4700777770000001,
>               "iterations": 1000,
>               "size": 80000
>             }
>           ]
>         },
>         {
>           "benchName": "Lazy List",
>           "samples": [
>             {
>               "average": 0.0019099180000000616,
>               "iterations": 1000,
>               "size": 0
>             },
>             {
>               "average": 0.002792022000000088,
>               "iterations": 1000,
>               "size": 20000
>             },
>             {
>               "average": 0.0022725489999997988,
>               "iterations": 1000,
>               "size": 40000
>             },
>             {
>               "average": 0.003802691999999979,
>               "iterations": 1000,
>               "size": 80000
>             }
>           ]
>         }
>       ],
>       "checkInputsResults": null,
>       "checkOutputsResults": null,
>       "groupName": "Replicate functions"
>     }
>   ],
>   "suiteName": "Minimal Example"
> }
> ```
<!-- end -->
