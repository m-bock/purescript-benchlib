module Test.Samples.Normalize where

import Prelude

import BenchLib (bench, benchAff, bench_, group, suite_)
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
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (range)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

main :: Effect Unit
main =
  BenchLib.runNode_ $
    suite_
      "Sample"

      [ group
          "Reverse collection"
          ( \cfg -> cfg
              { check = Just \size -> all \(input /\ output) ->
                  (input == range 0 size) && (output == Array.reverse input)

              }
          )
          [ bench
              "List"
              ( \opts -> opts
                  { normIn = List.toUnfoldable
                  , normOut = List.toUnfoldable
                  }
              )
              { prepare: \size -> range 0 size
              , run: \items -> List.reverse items
              }

          , bench
              "NonEmptyList"
              ( \opts -> opts
                  { normIn = NEL.toUnfoldable
                  , normOut = NEL.toUnfoldable
                  }
              )
              { prepare: \size -> range 0 size
              , run: \items -> NEL.reverse items
              }

          , bench
              "Lazy List"
              ( \opts -> opts
                  { normIn = LazyList.toUnfoldable
                  , normOut = LazyList.toUnfoldable
                  }
              )
              { prepare: \size -> range 0 size
              , run: \items -> LazyList.reverse items
              }

          , bench
              "Array"
              ( \opts -> opts
                  { normIn = identity
                  , normOut = identity
                  }
              )
              { prepare: \size -> range 0 size
              , run: \items -> Array.reverse items
              }

          , benchAff
              "Array"
              ( \opts -> opts
                  { iterations = 1
                  , normIn = \ref -> liftEffect $ Ref.read ref
                  , normOut = \ref -> liftEffect $ Ref.read ref
                  }
              )
              { prepare: \size -> liftEffect $ Ref.new (range 0 size)
              , run: \ref -> liftEffect do
                  --Ref.modify_ Array.reverse ref
                  pure ref
              }

          ]
      ]
