-----------------------------------------------------
-- Title: HOMEWORK 2 (Abstract Syntax)
-- Course: CS 381, Spring 2019
-- Authors: ADAM STEWART (stewaada), ANISH ASRANI (asrania), LUCAS FREY (freyl), MAZEN ALOTAIBI (alotaima), and SERGEI POLIAKOV(poliakos)
-----------------------------------------------------

----------------------------
-- Exercise 1.  Mini Logo --
----------------------------

-- a) Define the abstract syntax for Mini Logo as a Haskell data type.
data Cmd = Pen Mode
         | Moveto (Pos,Pos)
         | Def String Pars Cmd
         | Call Int Vals
         | Multiple Cmd Cmd
         deriving Show 
data Mode = Up | Down deriving Show
data Pos = PN Int | PS String deriving Show
data Pars = ManyS String Pars | S String deriving Show
data Vals = ManyN Int Vals | N Int deriving Show

-- b) Write a Mini Logo macro vector that draws a line from a given position (x1,y1) to a given position (x2,y2) and represent the macro in abstract syntax, that is, as a Haskell data type value.
vector = Def "vector" (S "draw line") (Multiple (Moveto (PS "x1", PS "y1")) (Multiple (Pen Down) (Multiple (Moveto (PS "x2", PS "y2")) (Pen Up))))


-- c) Define a Haskell function steps :: Int -$>$ Cmd that constructs a Mini Logo program which draws a stair of n step.
--moveStep :: Int -> Int -> Cmd -> Cmd
--moveStep 0 = Def "stairs" (S "draw lines") (Multiple (Moveto (PN 0, PN 0)) (Pen Down))

--steps :: Int -> Cmd
--steps x = moveStep 0

