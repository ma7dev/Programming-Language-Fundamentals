type Bag a = [(a, Int)]

-- a) Define the function ins that inserts an element into a multiset.
ins :: Eq a => a -> Bag a -> Bag a
ins new_elem [] = [(new_elem, 1)]
ins new_elem ((value, counter):tail) =
    if new_elem == value then do [(value, counter+1)] ++ tail
    else do [(value, counter)] ++ ins new_elem tail

-- b) Define the function del that removes an element from a multiset.
del :: Eq a => a -> Bag a -> Bag a
del target [] = []
del target ((value, counter):tail) =
    if value == target then do
        if counter <= 1 then do tail
        else do [(value, counter-1)] ++ tail
    else do [(value, counter)] ++ del target tail
     
-- c) Define a function bag that takes a list of values and produces a multiset representation.
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (head:tail) = ins head (bag tail)

-- d) Define a function subbag that determines whether or not its first argument bag is contained in the second.
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = False
subbag ((value, counter):tail) [] = False
subbag [] set = True
subbag ((value, counter):tail) set = 
    if del value set == set then do False
    else do
        if counter == 1 then do subbag tail set
        else do subbag ([(value, counter-1)] ++ tail) (del value set)

-- TO DEBUG
--subbagc :: Eq a => Bag a -> Bag a -> IO ()
--subbagc [] [] = putStrLn("[] [], TRUE")
--subbagc ((value, counter):tail) [] = 
--    if counter == 0 && tail == [] then do putStrLn("subset [], but TRUE")
--    else do putStrLn("subset [], FALSE")
--subbagc [] set = putStrLn("subset [], TRUE")
--subbagc ((value, counter):tail) set = 
--    if del value set == set then do
--        if counter == 0 then do putStrLn("wrong, next"); (subbagc tail set)
--        else do putStrLn("value not in set, FALSE")
--    else do
--        if counter == 0 then do putStrLn("next"); (subbagc tail set)
--        else do putStrLn("found, loop"); (subbagc ([(value, counter-1)] ++ tail) (del value set))
    
-- e) Define a function isbag that computes the intersection of two multisets.
isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag [] [] = []
isbag [] set_2 = []
isbag set_1 [] = []
isbag ((value, counter):tail) set_2 =
    if subbag ([(value, counter)] ++ tail) set_2 then do ([(value, counter)] ++ tail) 
    else if subbag [(value, counter)] set_2 then do isbag (tail ++ [(value, counter)]) ((del value set_2) ++ [(value, counter)])
    else do isbag ([(value, counter-1)] ++ tail) set_2
    
-- f) Define a function size that computes the number of elements contained in a bag.
size :: Bag a -> Int
size [] = 0
size set = sum (map snd set)

