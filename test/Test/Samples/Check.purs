module Test.Samples.Check where

import Prelude

import BenchLib (basic, bench, group, group_, reportConsole, suite, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml_)
import BenchLib.Reporters.Json (reportJson_)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Foldable (all)
import Data.List (List)
import Data.List as List
import Data.List.Lazy as Lazy
import Data.List.Lazy as LazyList
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (range, replicate)
import Effect (Effect)

main :: Effect Unit
main =
  BenchLib.run
    ( \cfg -> cfg
        { reporters =
            [ reportConsole -- Simply logs benchmarks to the console
            , reportJson_ -- Writes benchmarks to JSON file
            , reportHtml_ -- Writes benchmarks to HTML file
            ]
        }
    ) $
    suite_
      "Simple Example"

      [ group
          "Reverse collection"
          ( \cfg -> cfg
              { checkInputs = Just \{ results, size } ->
                  all (\result -> result == range 0 size) results

              , checkOutputs = Just \{ results, size } ->
                  all (\result -> result == Array.reverse (range 0 size)) results
              }

          )
          [ ( bench
                "List"
                ( \cfg -> cfg
                    { prepareInput = \(size :: Int) -> range 0 size
                    }
                )
                (\(items :: List Int) -> List.reverse items)
            )
              # bimap List.toUnfoldable List.toUnfoldable
              -- # updateCfg (\cfg ->)

          , ( bench
                "NonEmptyList"
                ( \cfg -> cfg
                    { prepareInput = \(size :: Int) -> range 0 size
                    }
                )
                (\(items :: NonEmptyList Int) -> NEL.reverse items)
            )
              # bimap NEL.toUnfoldable NEL.toUnfoldable

          , ( bench
                "Lazy List"
                ( \cfg -> cfg
                    { prepareInput = \(size :: Int) -> range 0 size
                    }
                )
                (\(items :: Lazy.List Int) -> LazyList.reverse items)
            )
              # bimap LazyList.toUnfoldable LazyList.toUnfoldable

          , ( bench
                "Array"
                ( \cfg -> cfg
                    { prepareInput = \(size :: Int) -> range 0 size
                    }
                )
                (\(items :: Array Int) -> Array.reverse items)
            )]
      ]
