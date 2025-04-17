module Test.Samples.PrepareFunction where

import Prelude

import BenchLib (bench_, group_, suite_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List as List
import Data.Unfoldable (replicate)
import Effect (Effect)

main :: Effect Unit
main =
  BenchLib.runNode_ $
    suite_
      "Sample"

      [ group_ "Get last element"
          [ bench_
              "List"
              { prepare: \size -> replicate size 'x'
              , run: \items -> List.last items
              }

          , bench_
              "Array"
              { prepare: \size -> replicate size 'x'
              , run: \items -> Array.last items
              }
          ]
      ]

