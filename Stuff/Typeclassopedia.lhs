
This is an implementation after the guide Typeclassopedia from the Haskell wiki.
See 'https://wiki.haskell.org/Typeclassopedia'

To avoid name clashes I use a prepending T(ypeclassopedia) in front
of all classes and some other things.

I expect the reader of this document to read the Typeclassopedia along-side.
This document is intended as a reference implementation of the code and examples.
For your own sake try to solve the problems yourself.
Only if you have no idea what to do look here.


> module Stuff.Typeclassopedia where

Type declaration for a Functor.

> class TFunctor f where
>   tfmap :: (a -> b) -> f a -> f b

 instance TFunctor [] where
   tfmap _ [] = []
   tfmap g (x:xs) = g x : fmap g xs

> instance TFunctor Maybe where
>   tfmap _ Nothing = Nothing
>   tfmap g (Just a) = Just (g a)

Functor exercises
1

> instance TFunctor (Either a) where
>   tfmap _ (Left e) = Left e
>   tfmap g (Right a) = Right (g a)
>
> instance TFunctor ((->) r) where
>   tfmap g h = (\x -> g (h x))
>

2

> instance TFunctor ((,) e) where
>   tfmap g ((,) e a) = ((,) e (g a))
>
> data Pair a = Pair a a deriving Show
> instance TFunctor Pair where
>   tfmap g (Pair a b) = Pair (g a) (g b)

I ommit the explanaitions. Do it yourself.
3

> data ITree a = Leaf (Int -> a) | Node [ITree a]
> instance TFunctor ITree where
>   tfmap g (Leaf a) = Leaf (g . a)
>   tfmap g (Node xs) = Node (map (tfmap g) xs)

4
Example of a type *->* which cannot made a Functor

> newtype T a = T (a -> Int)

It is not possible to write a fmap function for this

5
The composition of two Functors is also a Functor.
This can be shown by simply generating a Functor that consists of a List and a Maybe functor
E.g. f g = tfmap (tfmap g)

Functor Law Exercises
1 breaking the first law
2 what laws are violated

 instance TFunctor [] where
   tfmap _ [] = []
   tfmap g (x:xs) = g x : g x : tfmap g xs

Comment out the first instance for [] and comment in this one to try it out.
This tfmap definition only breaks the first law and holds the second.
try out with x = [1,2] g = (2+) and h = (2*) it hold on composition but not on id
