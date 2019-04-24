-----------------------------------------------------
-- Title: HOMEWORK 2 (Abstract Syntax)
-- Course: CS 381, Spring 2019
-- Authors: ADAM STEWART (stewaada), ANISH ASRANI (asrania), LUCAS FREY (freyl), MAZEN ALOTAIBI (alotaima), and SERGEI POLIAKOV(poliakos)
-----------------------------------------------------

--------------------------------------------------
-- Exercise 2.  Digital Circuit Design Language --
--------------------------------------------------

-- a) Define the abstract syntax for the above language as a Haskell data type.
data Circuit = C Gates Links deriving Show
data Gates = MultipleG Int GateFn Gates 
           | EmptyG
           deriving Show

data GateFn = And
            | Or 
            | Xor 
            | Not 
            deriving Show
data Links = MultipleL Int Int Int Int Links
           | EmptyL
           deriving Show

-- b) Represent the half adder circuit in abstract syntax, that is, as a Haskell data type value.
hac = C (MultipleG 1 Xor (MultipleG 2 And EmptyG)) (MultipleL 1 1 2 1 (MultipleL 1 2 2 2 EmptyL))

-- c) Define a Haskell function that implements a pretty printer for the abstract syntax.
printGateFn :: GateFn -> String
printGateFn And = "and"
printGateFn Or = "or"
printGateFn Xor = "xor"
printGateFn Not = "not"

printGates :: Gates -> String
printGates (MultipleG num gatefn gates) = show num ++ ":" ++ (printGateFn gatefn) ++ ";\n" ++ printGates gates
printGates EmptyG = ""

printLinks :: Links -> String
printLinks (MultipleL x1 y1 x2 y2 links) = "from " ++ show x1 ++ "." ++ show y1 ++ " to " ++ show x2 ++ "." ++ show y2 ++ ";\n" ++ printLinks links
printLinks EmptyL = ""

printHac :: Circuit -> IO()
printHac (C gates links) = putStr((printGates gates) ++ (printLinks links))


