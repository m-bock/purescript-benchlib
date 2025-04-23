module Test.Doc where

import Prelude

import BenchLib (Bench, Group, Size, Suite)
import BenchLib as BL
import Data.Array (all)
import Data.Array as Array
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))

prepareCharArray :: Size -> Array Char
prepareCharArray size = Array.replicate size 'x'

prepareCharList :: Size -> List Char
prepareCharList size = List.replicate size 'x'

prepareString :: Size -> String
prepareString size = String.fromCharArray (Array.replicate size 'x')

bench1 :: Bench Unit Unit
bench1 = BL.bench_
  "Length of Array of Char"
  { prepare: prepareCharArray
  , run: Array.length
  }

bench2 :: Bench Unit Unit
bench2 = BL.bench_
  "Length of List of Char"
  { prepare: prepareCharList
  , run: List.length
  }

bench3 :: Bench Unit Unit
bench3 = BL.bench_
  "Length of String"
  { prepare: prepareString
  , run: String.length
  }

group1 :: Group
group1 = BL.group_
  "Length operations"
  [ bench1
  , bench2
  , bench3
  ]

suite1 :: Suite
suite1 = BL.suite_
  "My Benchmarks"
  [ group1
  ]

-- benchNormalized1 :: Bench (Array Char) Int
-- benchNormalized1 = bench1

-- benchNormalized2 :: Bench (Array Char) Int
-- benchNormalized2 = BL.normalizeInput List.toUnfoldable bench2

-- benchNormalized3 :: Bench (Array Char) Int
-- benchNormalized3 = BL.normalizeInput String.toCharArray bench3

-- groupNormalized :: Group
-- groupNormalized = BL.group_
--   "Length of Char"
--   [ benchNormalized1
--   , benchNormalized2
--   , benchNormalized3
--   ]

-- suite1 ∷ Suite
-- suite1 = BL.suite_ "My Benchmarks"
--   [ groupNormalized
--   ]

-- suite2 ∷ Suite
-- suite2 =
--   BL.suite_
--     "My Benchmarks"
--     [ BL.group
--         "Length operations"
--         ( \opts -> opts
--             { sizes = [ 1, 10, 100, 1000 ]
--             , iterations = 1000

--             , check = Just \size -> all \(input /\ output) ->
--                 (input == Array.replicate size 'x') && (output == Array.length input)

--             }
--         )
--         [ BL.bench_
--             "Length of Array of Char"
--             (\size -> Array.replicate size 'x')
--             Array.length

--         , BL.normalizeInput List.toUnfoldable $ BL.bench_
--             "Length of List of Char"
--             (\size -> List.replicate size 'x')
--             List.length

--         , BL.normalizeInput String.toCharArray $ BL.bench_
--             "Length of String"
--             (\size -> String.fromCharArray (Array.replicate size 'x'))
--             String.length
--         ]
--     ]