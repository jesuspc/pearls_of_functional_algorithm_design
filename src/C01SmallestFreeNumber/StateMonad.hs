module C01SmallestFreeNumber.StateMonad (minfreeSM) where

import           Data.Array
import           Data.Array.ST

-- This implementation does not require elements in xs to be
-- distinct
minfreeSM :: [Int] -> Int
minfreeSM = search . checklist

-- Not every number in the range [0..length xs] can be in xs
-- => The smallest number not in xs is the smallest number not in
--    filter (<= n) xs where n = length xs

-- Build a checklist of the numbers present in filter (<= n) xs
-- The smallest free number will be the one

-- Using the state monad we have access to a constant time update
-- operation
checklist :: [Int] -> Array Int Bool
checklist xs = runSTArray(do
  a <- newArray (0, n) False
  sequence_ [writeArray a x True | x <- xs, x <= n]
  return a)
  where n = length xs

-- Returns the first false entry in the checklist
search :: Array Int Bool -> Int
search = length . takeWhile id . elems
