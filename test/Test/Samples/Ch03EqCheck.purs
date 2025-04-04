--- Header

module Test.Samples.Ch03EqCheck where

import Prelude

import BenchLib (bench, benchGroup, benchSuite_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.Foldable (all)
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..))
import Effect (Effect)

--- Main

main :: Effect Unit
main = BenchLib.run $
  benchSuite_
    "Minimal Example"
    [ benchGroup "Replicate Functions"
        ( \cfg -> cfg
            { checkOutputs =
                Just \{ outputs, size } -> all (_ == Array.replicate size 'x') outputs
            }
        )
        [ bench "Array"
            (\cfg -> cfg)
            (\size -> Array.replicate size 'x')

        , bench "Lazy List"
            ( \cfg -> cfg
                { finalize = LazyList.toUnfoldable
                }
            )
            (\size -> LazyList.replicate size 'x')
        ]
    ]

