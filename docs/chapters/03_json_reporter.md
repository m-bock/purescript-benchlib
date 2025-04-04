# The benchlib guide

## Configure Reporters

### Define the benchmark in PureScript

<!-- start:code
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
> import BenchLib (benchGroup_, benchSuite, bench_)
> import BenchLib as BenchLib
> import BenchLib.Reporters.Html (reportHtml)
> import BenchLib.Reporters.Json (reportJson)
> import BenchLib.Reporters.Markdown (reportMarkdown)
> import Data.Array as Array
> import Data.List.Lazy as LazyList
> import Effect (Effect)
> ```

</details>
<!-- end -->

<!-- start:code
{"file": "test/Test/Samples/MarkdownReporter.purs", "section": "Main"}
-->

> ```purescript
> main :: Effect Unit
> main = BenchLib.run $
>   benchSuite
>     "Minimal Example"
>     ( \cfg -> cfg
>         { iterations = 1000
>         , sizes = [ 0, 20_000, 40_000, 80_000 ]
>         , reporters = cfg.reporters <> reporters
>         }
>     )
>     [ benchGroup_ "Replicate functions"
>         [ bench_
>             "Array"
>             (\size -> const unit $ Array.replicate size 'x')
> 
>         , bench_
>             "Lazy List"
>             (\size -> const unit $ LazyList.replicate size 'x')
>         ]
>     ]
> ```
<!-- end -->

### Run the benchmark from Command Line

<!-- start:run
{"cmd": "npx spago run --main Test.Samples.MarkdownReporter", "hide": true}
-->
```bash
npx spago run --main Test.Samples.MarkdownReporter
```


<!-- end -->

### View the report

<!-- start:raw
{"file": "docs/chapters/02_reporters/report.md"}
-->
# Minimal Example
```mermaid
---
  config:
    themeVariables:
        xyChart:
            plotColorPalette: "#ff3456, #00ff00, #0000ff, #ffff00, #ff00ff, #00ffff"
---
xychart-beta
  title "Replicate functions"
  x-axis "Input Size" [0, 20000, 40000, 60000, 80000]
  y-axis "Time (in ms)" 0 --> 1
  line [0.003, 0.143, 0.247, 0.457, 0.003, 0.143, 0.247, 0.457]
  line [0.004, 0, 0, 0.001, 0.004, 0, 0, 0.001]
```
![ff3456](https://placehold.co/8x8/ff3456/ff3456.png) Array&nbsp;&nbsp;![00ff00](https://placehold.co/8x8/00ff00/00ff00.png) Lazy List
<!-- end -->
