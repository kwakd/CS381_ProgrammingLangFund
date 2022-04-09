module HW1_Elm exposing (..)

import HW1_Def exposing (..)

newtestVal = testVal

-- (a)define the function ins that inserts an element into a multiset
ins : a -> Bag a -> Bag a
ins x y = case y of
    [] -> [(x, 1)]
    n::xs ->  if fst n == x
        then [(x, (snd n) + 1)] ++ xs 
        else [n] ++ ins x xs
-- ins 5 newtestVal
-- [(5,4),(7,3),(2,1),(3,2),(8,1)]
-- GOAL: [(5,5),(7,3),(2,1),(3,2),(8,1)]


-- (b)define the function del that removes a single element from a multiset.
-- Note that deleting 3 from {2,3,3,4} yields {2,3,4} whereas deleting 3 from
-- {2,3,4} yields {2,4}

