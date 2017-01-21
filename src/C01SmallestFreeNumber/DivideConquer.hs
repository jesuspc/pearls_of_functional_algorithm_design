module C01SmallestFreeNumber.DivideConquer (minfreeDC) where

import           Data.Array

-- This implementation does require elements in xs to be distinct
minfreeDC :: [Int] -> Int
minfreeDC xs = undefined

(\\) :: (Eq a) => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us
