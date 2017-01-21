module C01SmallestFreeNumber.DivideConquer (minfreeDC) where

import           Data.List (partition)

-- This implementation does require elements in xs to be distinct

minfreeDC :: [Int] -> Int
minfreeDC xs = minfrom 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs)
  | n == 0 = a
  | m == b - a = minfrom b (n - m, xs2)
  | otherwise = minfrom a (m, xs1)
  where (xs1, xs2) = partition (< b) xs
        b = a + 1 + (n `div` 2)
        m = length xs1
