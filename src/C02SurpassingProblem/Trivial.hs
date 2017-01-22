module C02SurpassingProblem.Trivial (mscT, testData02) where

testData02 = "GENERATING"

mscT :: Ord a => [a] -> Int
mscT xs = maximum [scount z zs | z : zs <- tails xs]

scount :: Ord a => a -> [a] -> Int
scount x xs = length (filter (x <) xs)

tails :: [a] -> [[a]]
tails []     = []
tails (x:xs) = (x:xs) : tails xs
