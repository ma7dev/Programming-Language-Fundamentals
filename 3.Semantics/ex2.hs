module Ex2 where

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEF String Prog
         | CALL String
         deriving (Eq, Show)
         
    
type Stack  = [Int]
type Macros = [(String, Prog)]
type State  = (Macros, Stack)


data Output = StateOutput State 
            | StackOutput Stack
            | StackError
            | UndefinedMacroError deriving Show

findMacro :: String -> Macros -> Prog
findMacro "" _  = []
findMacro _  [] = []
findMacro name ((m_name, prog): ms) | name == m_name = prog
                                    | otherwise      = findMacro name ms

semCmd2 :: Cmd -> (Output -> Output)
semCmd2 (LD x) (StateOutput (macros, stack))    = StateOutput (macros, ([x] ++ stack))

semCmd2 (DUP)  (StateOutput (macros, []))       = StackError
semCmd2 (DUP)  (StateOutput (macros, stack))    = StateOutput (macros, ([head stack] ++ stack))

semCmd2 (ADD)  (StateOutput (macros, []))       = StackError
semCmd2 (ADD)  (StateOutput (macros, [_]))      = StackError
semCmd2 (ADD)  (StateOutput (macros, (x:y:xs))) = StateOutput (macros, ([x + y] ++ tail xs))

semCmd2 (MULT) (StateOutput (macros, []))       = StackError
semCmd2 (MULT) (StateOutput (macros, [_]))      = StackError
semCmd2 (MULT) (StateOutput (macros, (x:y:xs))) = StateOutput (macros, ([x * y] ++ tail xs))

semCmd2 (DEF name prog) (StateOutput (macros, stack)) = (StateOutput ([(name, prog)] ++ macros, stack))  
semCmd2 (CALL name) (StateOutput (ms, stack))
    | prog == [] = UndefinedMacroError
    | otherwise  = sem2 prog (StateOutput (ms, stack))
    where prog   = findMacro name ms 
                            
    
sem2 :: Prog -> (Output -> Output)
sem2 _      (StackError)          = StackError
sem2 _      (UndefinedMacroError) = UndefinedMacroError
sem2 []     (StateOutput xs)      = (StateOutput xs)
sem2 (p:ps) (StateOutput xs)      = sem2 ps (semCmd2 p (StateOutput xs))


--let test_def = DEF "test" [LD 1, LD 2]
--let test_state = StateOutput ([], [])
