module C01SmallestFreeNumber.ArrayBased (minfreeAB, testData) where

import           Data.Array

testData :: [Int]
testData = [0,1,5,4,7,2,6]

-- This implementation does not require elements in xs to be
-- distinct
minfreeAB :: [Int] -> Int
minfreeAB = search . checklist

-- Not every number in the range [0..length xs] can be in xs
-- => The smallest number not in xs is the smallest number not in
--    filter (<= n) xs where n = length xs

-- Build a checklist of the numbers present in filter (<= n) xs
-- The smallest free number will be the one

-- Populates with False by default an array of n elements
-- Populates with True all elements with indices taken from xs
-- If indices repeated it uses the || operation so it can handle
-- the conflict.
-- This operation is O(n)
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) assocList
               where n = length xs
                     assocList = zip (filter (<= n) xs) (repeat True)

-- Returns the first false entry in the checklist
search :: Array Int Bool -> Int
search = length . takeWhile id . elems
