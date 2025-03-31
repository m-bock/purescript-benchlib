module Test.Specs where

import Prelude

import BenchLib (benchGroup_, benchM_, benchSuite, bench_, eval)
import BenchLib as Bench
import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Partial.Unsafe (unsafePartial)
import BenchLib (benchSuite_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "1" do
    it "empty ResizeArray" do
      ret <- eval $
        benchSuite_ "suite1"
          [ benchGroup_ "range"
              [ bench_ "short range" (\_ -> Array.range 0 10)
              , bench_ "long range" (\_ -> Array.range 0 100_000)
              ]
          ]
      pure unit



  describe "1" do
    it "" do
      ret <- eval $
        benchSuite "suite1"
          ( \_ ->
              { sizes: [ 5, 10 ]
              , iterations: 10
              , reporters: []
              }
          )
          [ benchGroup_ "group1"
              [ benchM_ "test1" (\_ -> delay (Milliseconds 10.0))
              , benchM_ "test2" (\_ -> delay (Milliseconds 15.0))
              ]
          --   , benchGroup_ "group2"
          --       [ bench_ "test1" (\_ -> pure 1 :: Aff _)
          --       , bench_ "test2" (\_ -> pure 1 :: Aff _)
          --       ]
          ]

      ret.suiteName
        `shouldEqual` "suite1"

      Array.length ret.groups
        `shouldEqual` 1

      let
        at :: forall a. Int -> Array a -> a
        at i xs = unsafePartial $ Array.unsafeIndex xs i

      do
        (ret.groups # at 0 # _.groupName)
          `shouldEqual` "group1"

        do
          (ret.groups # at 0 # _.benchs # at 0 # _.benchName)
            `shouldEqual` "test1"

          (ret.groups # at 0 # _.benchs # at 0 # _.size)
            `shouldEqual` 5

          --   (ret.groups # at 0 # _.benchs # at 0 # _.duration)
          --     `shouldEqual` (Milliseconds 0.3)

          (ret.groups # at 0 # _.benchs # at 0 # _.iterations)
            `shouldEqual` 10

      ret `shouldEqual`
        { suiteName: "suite1"
        , groups:
            [ { groupName: "group1"
              , benchs:
                  [ { benchName: "test1"
                    , size: 1
                    , duration: Milliseconds 0.3
                    , iterations: 1
                    }
                  ]
              }
            ]
        }

