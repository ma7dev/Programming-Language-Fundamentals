module Ex1 where 

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving Show
         
    
type Stack = [Int]

data Output = S Stack | Error deriving Show

semCmd :: Cmd -> (Output -> Output)
semCmd (LD x) (S xs)     = S ([x] ++ xs)
semCmd (DUP)  (S [])     = Error
semCmd (DUP)  (S xs)     = S ([head xs] ++ xs)
semCmd (ADD)  (S [])     = Error
semCmd (ADD)  (S [_])    = Error
semCmd (ADD)  (S (x:xs)) = S ([x + head xs] ++ tail xs)
semCmd (MULT) (S [])     = Error
semCmd (MULT) (S [_])    = Error
semCmd (MULT) (S (x:xs)) = S ([x * head xs] ++ tail xs)

sem :: Prog -> (Output -> Output)
sem _      (Error) = Error
sem []     (S xs)  = (S xs)
sem (p:ps) (S xs)  = sem ps (semCmd p (S xs))
