# The benchlib guide

## Configure Reporters

### Define the benchmark in PureScript

<!-- start:code
{ 
  "file": "test/Test/Samples/MarkdownReporter.purs",
  "section": "Header",
  "link": true
}
-->
Source Code: [test/Test/Samples/MarkdownReporter.purs](test/Test/Samples/MarkdownReporter.purs)
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
<!-- end -->


<!-- start:code
{"file": "test/Test/Samples/MarkdownReporter.purs", "section": "Reporters"}
-->

> ```purescript
> reporters :: Array BenchLib.Reporter
> reporters =
>   [ reportMarkdown \cfg -> cfg
>       { filePath = "docs/chapters/02_reporters/report.md"
>       , showHeadline = true
>       }
> 
>   , reportJson \cfg -> cfg
>       { filePath = "docs/chapters/02_reporters/report.json"
>       , indent = 2
>       }
> 
>   , reportHtml \cfg -> cfg
>       { filePath = "docs/chapters/02_reporters/report.html"
>       , lineStyles =
>           [ { color: { r: 255, g: 99, b: 132 }, opacity: 0.5, width: 2.0 }
>           , { color: { r: 54, g: 162, b: 235 }, opacity: 0.5, width: 2.0 }
>           , { color: { r: 153, g: 102, b: 255 }, opacity: 0.5, width: 2.0 }
>           ]
>       }
>   ]
> ```
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

You can have a look at the generated reports:

[report.md](docs/chapters/02_reporters/report.md)
[report.json](docs/chapters/02_reporters/report.json)
[report.html](docs/chapters/02_reporters/report.html)