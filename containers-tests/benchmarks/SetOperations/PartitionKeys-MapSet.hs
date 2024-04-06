module Main where

import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

import qualified Data.Map as M
import qualified Data.Set as S
import SetOperations
import Utils.Containers.Internal.StrictPair

main =
  benchmark'
    (\xs -> M.fromList [(x, x) | x <- xs])
    S.fromList
    True
    [ (baseName, M.partitionKeys, const id)
    , ("partitionKeysSplitMap", M.partitionKeysSplitMap, \subname -> mapLeafBenchmarks (addCompare (baseName ++ subname)))
    ]
  where
    baseName = "partitionKeys"

addCompare :: String -> [String] -> Benchmark -> Benchmark
addCompare targetBenchName (name : path)
  | name /= targetBenchName
  = bcompare (printAwkExpr (locateBenchmark (targetBenchName : path)))
addCompare _ _ = id
