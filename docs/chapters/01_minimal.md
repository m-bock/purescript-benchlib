# Minimal example

## Define the benchmark in PureScript

### Module Imports
<!-- start:pursCode
{ 
  "file": "test/Test/Samples/Minimal.purs",
  "section": "Header",
  "link": true
}
-->
Source Code: [test/Test/Samples/Minimal.purs](test/Test/Samples/Minimal.purs)
> ```purescript
> module Test.Samples.Minimal where
> 
> import Prelude
> 
> import BenchLib (basic, group_, suite_, bench_)
> import BenchLib as BenchLib
> import Data.Array as Array
> import Data.List.Lazy as LazyList
> import Effect (Effect)
> ```
<!-- end -->

### Blas

In the `main` function, we define a benchmark suite with a single group and two benchmarks. Each benchmark is defined using the `bench_` function, which takes a name and a function that generates the data to be benchmarked.

<!-- start:pursCode
{"file": "test/Test/Samples/Minimal.purs", "section": "Main"}
-->

> ```purescript
> main :: Effect Unit
> main = BenchLib.run_ $
>   suite_
>     "Minimal Example"
>     [ group_ "Replicate Functions"
>         [ basic $ bench_
>             "Array"
>             (\size -> Array.replicate size 'x')
> 
>         , basic $ bench_
>             "Lazy List"
>             (\size -> LazyList.replicate size 'x')
>         ]
>     ]
> ```
<!-- end -->


## Run the benchmark from Command Line

After defining the benchmark, you can run it from the command line using the `spago` command. The `--main` flag specifies the module containing the `main` function.

<!-- start:run
{
  "cmd": "npx spago run --main Test.Samples.Minimal",
  "text": "The output will look something like this:"
}
-->
```bash
npx spago run --main Test.Samples.Minimal
```
The output will look something like this:

```text
• suite: Minimal Example
  • group: Replicate Functions
    • bench: Array
      • size=0, count=1000, avg=0.0024ms
      • size=25000, count=1000, avg=0.1735ms
      • size=50000, count=1000, avg=0.4148ms
      • size=100000, count=1000, avg=0.5978ms
    • bench: Lazy List
      • size=0, count=1000, avg=0.0018ms
      • size=25000, count=1000, avg=0.0029ms
      • size=50000, count=1000, avg=0.0023ms
      • size=100000, count=1000, avg=0.0020ms
Suite finished
```
<!-- end -->

The results show what you might have already guessed: the `Array` implementation is significantly slower than the `Lazy List` implementation. The reason is obvious: The lazy list is not evaluated until it is needed, so it is much faster to create.

You can also see that the benchmarks have been running for different input sizes, and each benchmark has been run for several iterations. These values can be configured and in the next chapters we will show you how to do that.