-- Sergei Poliakov

module Homework4E1 where

import Data.List

-- Exercise 1 A Rank-Based Type Systems for the Stack Language

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int
         deriving (Eq, Show)

type Prog = [Cmd]

type Stack = [Int]

type D = Stack -> Stack

semCmdHelper:: Cmd -> D
semCmdHelper (ADD)  (x:y:xs) = ((x+y):xs)
semCmdHelper (MULT) (x:y:xs) = ((x*y):xs)
semCmdHelper (DUP)  (x:xs)   = ([x,x] ++ xs)
semCmdHelper (INC) (x:xs) = ((x+1):xs)
semCmdHelper (SWAP) (x:y:xs) = (y:x:xs)
semCmdHelper (POP x) xs = drop x (reverse xs)


semCmd :: Cmd -> D
semCmd (LD x) xs = xs ++ [x]
semCmd (ADD)  x | (length x) == 0 || (length x) == 1 = error ("CANNOT ADD")
                | otherwise = semCmdHelper (ADD) x
semCmd (MULT) x | (length x) == 0 || (length x) == 1 = error ("CANNOT MULT")
                | otherwise = semCmdHelper (MULT) x
semCmd (DUP)  x | (length x) == 0 = error ("NOTHING TO DUPLICATE")
                | otherwise = semCmdHelper (DUP) x
semCmd (INC)  x | (length x) == 0 = error ("NOTHING TO INCREMENT")
                | otherwise = semCmdHelper (INC) x
semCmd (SWAP) x | (length x) == 0 || (length x) == 1 = error ("CANNOT SWAP")
                | otherwise = semCmdHelper (SWAP) x
semCmd (POP x) xs | (length xs) < x = error ("CANNOT POP")
                  | otherwise = semCmdHelper (POP x) xs

sem :: Prog -> D
sem [] y = y
sem (x:xs) y = sem xs (semCmd x y)

-- compute:: Prog -> Stack
-- compute p = sem p ([])

-- a)
type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD _)  = (0,1)
rankC (ADD)   = (2,1)
rankC (MULT)  = (2,1)
rankC (DUP)   = (1,2)
rankC (INC)   = (1,1)
rankC (SWAP)  = (2,2)
rankC (POP x) = (x,0)

rank :: Prog -> Rank -> Maybe Rank
rank [] x | x >= 0 = Just x
          | otherwise = Nothing
rank (x:xs) y = rank xs ((y-n)+m)
                where n = fst(rankC x)
                      m = snd(rankC x)

rankP :: Prog -> Maybe Rank
rankP p = rank p 0

-- b)

typeCorrect :: Prog -> Bool
typeCorrect e = rankP e /= Nothing

semStatTC :: Prog -> Maybe Stack
semStatTC e | typeCorrect e = Just (sem e ([]))
            | otherwise = Nothing

