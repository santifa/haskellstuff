> module Stuff.ProjectEuler.ProjectEuler where
> import Data.List
> import Data.Numbers.Primes

Problem 1:

Generates the sum of all numbers with mod 3 or 5 equals zero.
for all x, x < t and x mod 3 == 0 and x mod 5 == 0 . x, t in N

Sum of all mulitplies of 5 and 3 below 1000
< problem1 999

> problem1 :: Integer -> Integer
> problem1 x = sum . nub $ filter (\y -> y `mod` 3 == 0 || y `mod` 5 == 0) [1..x]

Problem 2:

Generate the sum of all even-valued fibonacci numbers below 4 millionen.
< problem2 4000000

> problem2 :: Integer -> Integer
> problem2 x = sum . filter even $ takeWhile (<x) fib
> fib :: [Integer]
> fib = fibs (0,1)
>   where fibs (a,b) = a:fibs (b,a+b)

Problem 3:

Largest prime factor of the number 600851475143
< problem3 600581475143

> problem3 :: Integer -> Integer
> problem3 x = last $ primeFactors x

Problem 4:

Find the largest palindrome made of two 3-digit numbers

> problem4 :: String
> problem4 = last . sort $ filter palin [show (x * y)| x <- [100..999], y <- [100..999]]
>   where palin [] = True
>         palin (_:[]) = False
>         palin (x:xs)
>           | x /= last xs = False
>           | otherwise = palin $ init xs

Problem 5:

What is the smallest positive number that can be devided by all numbers from 1 to 20 without any remainder

 problem5 :: Integer

> problem5 :: Integer
> problem5 = head [x | x <- [2,4..], x `mod` 11 == 0, x `mod` 12 == 0, x `mod` 13 == 0, 
>   x `mod` 14 == 0, x `mod` 15 == 0, x `mod` 16 == 0, x `mod` 17 == 0, 
>   x `mod` 18 == 0, x `mod` 19 == 0, x `mod` 20 == 0]



