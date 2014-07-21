> module CommonTuple where

Common tuple funktion
The tuple function is bijective function that creates from
a pair (x,y) a number n. The common tuple is used to create pairs
in the theorie of primitiv recursion.

The scheme is following:

  :
  4 | 14 ...
  3 | 9 13 ...
  2 | 5 8 12 ...
  1 | 2 4 7 11 ...
  0 | 0 1 3 6 10 ...
  x +_____________
    y 0 1 2 3 4 ...

The function is declared as
<x,y> = (\Sigma(from 1 to x+y) i) + y ->
<x,y> = ((x+y) (x+y+1)) / 2 + y

The reverse function is declared as
\pi(n over k) := pr(n over k) * <>^(-1)
where n over k is the kth from n inputs and pr is the projection and
<> is the common tuple function.

More mathematical
z n = n * (n + 1) / 2
w n = sqrt (8 * n + 1) - 1 / 2
\pi(2 over 2) n = n - (z (w n)) -- this gives us the y
\pi(2 over 1) n = w n - \pi(2 over 2) n -- this is our x


A help function that allows us to sqrt integers

> sqrt' :: Integer -> Integer
> sqrt' = floor . sqrt . (fromIntegral :: Integer -> Double)

The common tuple function

> ct' :: Integer -> Integer -> Integer
> ct' x y = xy * (xy + 1) `quot` 2 + y
>          where xy = x + y

And a handy calling function

> ct :: Integer -> Integer -> String
> ct x y = "<" ++ (show x) ++ "," ++ (show y) ++ "> -> n: " ++ (show (ct' x y))

The reverse function

> z :: Integer -> Integer
> z n = (n * (n + 1)) `quot` 2

> w :: Integer -> Integer
> w n = (sqrt' (8 * n + 1) - 1) `quot` 2

The snd' function gives us the second number.

> snd' :: Integer -> Integer
> snd' n = n - (z (w n))

The fst' function gives us the first number.

> fst' :: Integer -> Integer
> fst' n = (w n) - snd' n

This function gives us our tuple.

> rev :: Integer -> String
> rev n = "n: " ++ (show n) ++ " -> x: " ++
>         show (fst' n) ++ " y: " ++ show (snd' n)
