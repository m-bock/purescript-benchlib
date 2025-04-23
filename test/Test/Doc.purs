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
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)

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

fullExample1 :: Effect Unit
fullExample1 = BL.runNode_ $
  BL.suite_
    "Sample"
    [ BL.group_
        "Length operations"
        [ BL.bench_
            "Length of Array of Char"
            { prepare: \size -> Array.replicate size 'x'
            , run: Array.length
            }
        , BL.bench_
            "Length of List of Char"
            { prepare: \size -> List.replicate size 'x'
            , run: List.length
            }
        , BL.bench_
            "Length of String"
            { prepare: \size -> String.fromCharArray (Array.replicate size 'x')
            , run: String.length
            }
        ]
    ]

benchNormalized1 :: Bench (Array Char) Int
benchNormalized1 = BL.bench
  "Length of Array of Char"
  ( \opts -> opts
      { normIn = identity
      , normOut = identity
      }
  )
  { prepare: \size -> Array.replicate size 'x'
  , run: Array.length
  }

benchNormalized2 :: Bench (Array Char) Int
benchNormalized2 = BL.bench
  "Length of List of Char"
  ( \opts -> opts
      { normIn = List.toUnfoldable
      , normOut = identity
      }
  )
  { prepare: \size -> List.replicate size 'x'
  , run: List.length
  }

benchNormalized3 :: Bench (Array Char) Int
benchNormalized3 = BL.bench
  "Length of String"
  ( \opts -> opts
      { normIn = String.toCharArray
      , normOut = identity
      }
  )
  { prepare: \size -> String.fromCharArray (Array.replicate size 'x')
  , run: String.length
  }

checkFn :: Int -> Array (Array Char /\ Int) -> Boolean
checkFn size = all \(input /\ output) ->
  (input == Array.replicate size 'x') && (output == Array.length input)

groupNormalized :: Group
groupNormalized = BL.group
  "Length of Char"
  ( \cfg -> cfg
      { check = Just checkFn
      }
  )
  [ benchNormalized1
  , benchNormalized2
  , benchNormalized3
  ]
