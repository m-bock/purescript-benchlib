module Test.Samples.PrepareFunction where

import Prelude

import BenchLib (basic, bench, group_, suite_)
import BenchLib as BenchLib
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Unfoldable (replicate)
import Effect (Effect)

-- main :: Effect Unit
-- main =
--   BenchLib.runNode_ $
--     suite_
--       "Sample"

--       [ group_
--           "Get last element"
--           [ basic $ bench
--               "List"
--               ( \cfg -> cfg
--                   { prepareInput = \(size :: Int) -> replicate size 'x'
--                   }
--               )
--               (\(items :: List Char) -> List.last items)

--           , basic $ bench
--               "Array"
--               ( \cfg -> cfg
--                   { prepareInput = \(size :: Int) -> replicate size 'x'
--                   }
--               )
--               (\(items :: Array Char) -> Array.last items)
--           ]
--       ]

