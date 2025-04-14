module Test.Samples.Check where

import Prelude

import BenchLib (bench, bench_, group, normalize, suite_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.Foldable (all)
import Data.List (List)
import Data.List as List
import Data.List.Lazy as Lazy
import Data.List.Lazy as LazyList
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (range)
import Effect (Effect)

main :: Effect Unit
main =
  BenchLib.runNode_ $
    suite_
      "Sample"

      [ group
          "Reverse collection"
          ( \cfg -> cfg
              { checkInputs = Just \{ results, size } ->
                  all (\result -> result == range 0 size) (results :: Array _)

              , checkOutputs = Just \{ results, size } ->
                  all (\result -> result == Array.reverse (range 0 size)) (results :: Array _)
              }

          )
          [ normalize List.toUnfoldable List.toUnfoldable $ bench_
              "List"
              (\(size :: Int) -> range 0 size)
              (\(items :: List Int) -> List.reverse items)

          , normalize NEL.toUnfoldable NEL.toUnfoldable $ bench_
              "NonEmptyList"
              (\(size :: Int) -> range 0 size)
              (\(items :: NonEmptyList Int) -> NEL.reverse items)

          , normalize LazyList.toUnfoldable LazyList.toUnfoldable $ bench_
              "Lazy List"
              ( \(size :: Int) -> range 0 size)
              (\(items :: Lazy.List Int) -> LazyList.reverse items)

          , bench_
              "Array"
              ( \(size :: Int) -> range 0 size)
              (\(items :: Array Int) -> Array.reverse items)
          ]
      ]
