module Ex3 where

data Cmd = Pen Mode
         | MoveTo Int Int
         | Seq Cmd Cmd
         deriving Show

data Mode = Up
          | Down
          deriving (Eq, Show)

type State = (Mode, Int, Int)
type Line  = (Int,  Int, Int, Int)
type Lines = [Line]


semS :: Cmd -> State -> (State, Lines)
semS (Pen nm)       (cm, x, y)   = ((nm , x, y), [])
semS (MoveTo x2 y2) (cm, x1, y1) | cm == Down = ((cm ,x2 ,y2 ), [(x1 , y1, x2, y2)])
                                 | otherwise  = ((cm , x2, y2), [])
semS (Seq c1 c2)    (cm, x, y)   = (s2, l1 ++ l2)
                                   where (s2, l2) = semS c2 s1
                                         (s1, l1) = semS c1 (cm, x, y)
 

sem' :: Cmd -> Lines
sem' x = snd (semS x (Up, 0, 0))

