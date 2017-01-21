module C01SmallestFreeNumber.Trivial (minfreeT) where

import           Data.Array

-- This implementation does not require elements in xs to be distinct
minfreeT :: [Int] -> Int
minfreeT xs = head ([0..] \\ xs)

(\\) :: (Eq a) => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us
