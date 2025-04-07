--- Header

module Test.Samples.Ch03EqCheck where

import Prelude

import BenchLib (bench, benchGroup, benchSuite_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Foldable (all)
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..))
import Effect (Effect)

--- Main

main :: Effect Unit
main = BenchLib.run_ $
  benchSuite_
    "Minimal Example"
    [ benchGroup "Replicate Functions"
        ( \cfg -> cfg
            { checkOutputs =
                Just \{ values, size } -> all (_ == Array.replicate size 'x') values
            , checkInputs =
                Just \{ values, size } -> all (_ == Array.replicate size 'x') values
            }
        )
        [ bench
            "Array"
            ( \cfg -> cfg
                { prepareInput = \size -> Array.replicate size 'x'

                }
            )
            (\xs -> Array.snoc xs 'y')

        , bimap Array.fromFoldable Array.fromFoldable $ bench
            "Lazy List"
            ( \cfg -> cfg
                { prepareInput = \size -> LazyList.replicate size 'x'
                }
            )
            (\xs -> LazyList.cons 'y' xs)
        ]
    ]

