-----------------------------------------------------
-- Title: HOMEWORK 2 (Abstract Syntax)
-- Course: CS 381, Spring 2019
-- Authors: ADAM STEWART (stewaada), ANISH ASRANI (asrania), LUCAS FREY (freyl), MAZEN ALOTAIBI (alotaima), and SERGEI POLIAKOV(poliakos)
-----------------------------------------------------

--------------------------------------------
-- Exercise 3.  Designing Abstract Syntax --
--------------------------------------------

data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
          deriving Show

data Op = Add | Multiply | Negate deriving Show
data Exp = Num Int
         | Apply Op [Exp]
         deriving Show

-- a) Represent the expression -(3+4)*7 in the alternative abstract syntax.
as = Apply Multiply [Apply Negate [Apply Add [Num 3,Num 4]],Num 7]
bs = Times (Neg (Plus (N 3) (N 4))) (N 7)

-- b) What are the advantages or disadvantages of either representation?
-- look into the document

-- c) Define a function translate :: Expr -> Exp that translates expressions given in the first abstract syntax into equivalent expressions in the second abstract syntax
translate :: Expr -> Exp
translate (N x) = (Num x)
translate (Plus x y) = (Apply Add [(translate x), (translate y)])
translate (Times x y) = (Apply Multiply [(translate x), (translate y)])
translate (Neg x) = (Apply Negate [(translate x)]) 
