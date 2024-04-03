module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import SetOperations
import Utils.Containers.Internal.StrictPair

main =
  benchmark'
    (\xs -> M.fromList [(x, x) | x <- xs])
    S.fromList
    True
    [ ("partitionKeys", M.partitionKeys)
    , ("partitionKeysSplitMap", M.partitionKeysSplitMap)
    ]
