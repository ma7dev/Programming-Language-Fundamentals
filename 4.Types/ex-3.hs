-- Sergei Poliakov

module Homework4E3 where

import Data.List

-- Exercise 3 Parametric Polymorphism

-- a)

f x y = if null x then [y] else x

g x y = if not (null x) then [] else [y]
g [] y = []
{-
(1) What are the types of f and g?
     f :: [a] -> a -> [a]
     g :: [a] -> b -> [b]

(2) Explain why the functions have these types.
     Both functions take in two parameters, the
     first being a list of "a's" and the second
     parameter being either a value "a" or "b",
     depending on the function. Then both functions
     return a list of "a's".

(3) Which type is more general?
     The type of the function g is more general,
     because the output of the function will
     always be a list, regardless whether the
     condition is met or not.

(4) Why do f and g have different types?
     Function g has a different type from f, because
     in g, x is never part of the output.
-}

-- b)
h :: [b] -> [(a,b)] -> [b]
h x _ = x

-- c)

k :: (a -> b) -> ((a -> b) -> a) -> b
k x y = x (y x)
-- d)
{- The function of type a -> b is difficult to defince because
   not enough information is given about type b. Therefore
   we cannot produce an accurate function defenition, that will
   perform the proper type conversions. -}

