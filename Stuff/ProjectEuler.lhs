> module ProjectEuler where
> import Data.List (nub)

Problem 1:

Generates the sum of all numbers with mod 3 or 5 equals zero.
for all x, x < t and x mod 3 == 0 and x mod 5 == 0 . x, t in N

Sum of all mulitplies of 5 and 3 below 1000
< problem1 999

> problem1 :: Integer -> Integer
> problem1 x = sum . nub $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..x]
