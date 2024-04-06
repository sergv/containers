{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SetOperations (benchmark, benchmark') where

import Test.Tasty.Bench (Benchmark, bench, defaultMain, whnf)
import Data.List (partition, sortBy)
import Data.Ord (comparing)
import Data.Tuple as Tuple

-- | Benchmark a set operation for the given container.
-- Takes the following arguments:
-- * A way to construct the container
-- * Flag if we should benchmark the operations with reversed arguments.
-- * A list of operations.
benchmark :: forall container. (Show container, Eq container) => ([Int] -> container) -> Bool -> [(String, container -> container -> container)] -> IO ()
benchmark fromList swap methods =
  benchmark' fromList fromList swap (map (\(a, b) -> (a, b, const id)) methods)

benchmark'
  :: forall container1 container2 container3.
     (Show container1, Eq container1, Show container2, Eq container2, Show container3, Eq container3)
  => ([Int] -> container1)
  -> ([Int] -> container2)
  -> Bool
  -> [(String, container1 -> container2 -> container3, String -> Benchmark -> Benchmark)]
  -> IO ()
benchmark' fromList1 fromList2 swap methods = do

  defaultMain $ [ f subname $ bench (method_str ++ subname) $
                  whnf (method input1) input2

                | (method_str, method, f) <- methods
                , (input_str, data_sizes, (input1, input2)) <- sortBenchs all_inputs
                , let subname = "-" ++ input_str ++ "_" ++ data_sizes
                ]

  where
    -- Sort benchmark inputs by (data variant, data sizes)
    sortBenchs = sortBy (comparing (\(name,size,_) -> (name,size)))

    -- Data size descriptions, also used in the benchmark names.
    -- They are used to describe how large the input data is, but NOT the data itself.
    -- So for example nn_swap /= nn since the data size for both arguments is the same
    -- but the actual data is different.
    n, s, t :: Int
    n = 100000
    s {-small-} = n `div` 10
    t {-tiny-} = round $ sqrt $ fromIntegral n

    all_inputs :: [(String, String, (container1, container2))]
    all_inputs = map (\(a, b, c) -> (a, b, fromLists c)) $ base_inputs ++ swapped_input

    base_inputs :: [(String, String, ([Int], [Int]))]
    base_inputs =
      [ ("disj",   "nn", disj_nn)
      , ("disj",   "ns", disj_ns)
      , ("disj",   "nt", disj_nt)
      , ("common", "nn", common_nn)
      , ("common", "ns", common_ns)
      , ("common", "nt", common_nt)
      , ("mix",    "nn", mix_nn)
      , ("mix",    "ns", mix_ns)
      , ("mix",    "nt", mix_nt)
      , ("block",  "nn", block_nn)
      , ("block",  "ns", block_ns)
      ]

    -- Input with set arguments swapped.
    swapped_input
      | swap = map swap_input base_inputs
      | otherwise = []

    -- Reverse arguments
    swap_input (name, data_sizes, input_data) =
        (name, reverse data_sizes ++ "_swap", Tuple.swap input_data)

    -- Data variants
    all_n = [1..n]

    !disj_nn   = (all_n, [n+1..n+n])
    !disj_ns   = (all_n, [n+1..n+s])
    !disj_nt   = (all_n, [n+1..n+t])
    !common_nn = (all_n, [2,4..n])
    !common_ns = (all_n, [0,1+n`div`s..n])
    !common_nt = (all_n, [0,1+n`div`t..n])
    !mix_nn    = partition ((/= 0) . (`mod` 2)) [1..n+n]
    !mix_ns    = partition ((/= 0) . (`mod` (1 + n`div`s))) [1..s+n]
    !mix_nt    = partition ((/= 0) . (`mod` (1 + n`div`t))) [1..t+n]
    !block_nn  = partition ((>= t) . (`mod` (t * 2))) [1..n+n]
    !block_ns  = partition ((>= t) . (`mod` (t * (1 + n`div`s)))) [1..s+n]

    fromLists (xs, ys) = seqPair (fromList1 xs, fromList2 ys)
    seqPair pair@(xs, ys) = xs `seq` ys `seq` pair
