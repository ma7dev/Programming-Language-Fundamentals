-- 
-- ex2
--
module HW4E2 where
import Data.Tuple
import Data.Maybe

data Shape = X 
            | TD Shape Shape
            | LR Shape Shape
            deriving Show

type BBox = (Int, Int)

-- (a) 

bbox :: Shape -> BBox

bbox X              = (1, 1)
bbox (LR s1 s2)     = (x1+x2, max y1 y2)
                        where (x1, y1) = bbox s1
                              (x2, y2) = bbox s2

bbox (TD s1 s2)     = (max x1 x2, y1+y2)
                        where (x1, y1) = bbox s1
                              (x2, y2) = bbox s2

                    
-- (b)

rect :: Shape -> Maybe BBox

rect X                      = Just (1,1)

rect (LR s1 s2) | y1==y2 = Just (x1+x2, y2)
                | otherwise = Nothing
                        where (x1, y1) = bbox s1
                              (x2, y2) = bbox s2
                       
rect (TD s1 s2) | x1==x2 = Just (x1, y1+y2)
                | otherwise = Nothing
                        where (x1, y1) = bbox s1
                              (x2, y2) = bbox2
                       
