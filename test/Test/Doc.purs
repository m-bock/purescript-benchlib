module Test.Doc where

import Prelude

import BenchLib (Bench, BenchOpts(..), Group, Size(..), Size, bench, benchAff, bench_)
import BenchLib as BL
import Data.Array as Array
import Data.Generic.Rep (from)
import Data.List.Lazy (List(..))
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.CodeUnits as String

prepareCharArray :: Size -> Array Char
prepareCharArray size = Array.replicate size 'x'

prepareCharList :: Size -> List Char
prepareCharList size = List.replicate size 'x'

prepareString :: Size -> String
prepareString size = String.fromCharArray (Array.replicate size 'x')

b1 :: Bench (Array Char) Int
b1 = BL.bench "Length of Array of Char"
  ( \opts -> opts
      { iterations = 1000
      , prepareInput = prepareCharArray
      }
  )
  Array.length

b2 :: Bench (List Char) Int
b2 = BL.bench "Length of List of Char"
  ( \opts -> opts
      { iterations = 1000
      , prepareInput = prepareCharList
      }
  )
  List.length

b3 :: Bench String Int
b3 = BL.bench "Length of String"
  ( \opts -> opts
      { iterations = 1000
      , prepareInput = prepareString
      }
  )
  String.length

b1' :: Bench Unit Unit
b1' = BL.basic b1

b2' :: Bench Unit Unit
b2' = BL.basic b2

b3' :: Bench Unit Unit
b3' = BL.basic b3

groupBasic :: Group
groupBasic = BL.group_
  "Length of Char"
  [ b1', b2', b3' ]