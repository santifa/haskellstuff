Generates the sum of all numbers with mod 3 or 5 equals zero.
for all x, x < t and x mod 3 == 0 and x mod 5 == 0 . x, t in N
import Data.List

> main = print (prob1 999 999)

Sum of all mulitplies of 5 and 3 below 1000

> prob1 :: Integer -> Integer -> Integer
> prob1 x y = sum ( nub ([x | x <- [1..x], x `mod` 3 == 0] ++ [y | y <- [1..y], y `mod` 5 == 0]))
