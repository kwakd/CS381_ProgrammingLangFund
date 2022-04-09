module HW1_Elm exposing (..)

import HW1_Def exposing (..)
import List exposing (..)

----------------------------------------
-- CS 381 HW1 Contributors
----------------------------------------
-- SPRING 2022
----------------------------------------
-- Fatimah Alsalman  
-- Travis Hudson 
-- Manju Kuah   
-- David Kwak
----------------------------------------
map = List.map

type alias Number = Int
type alias Point = (Number,Number)
type alias Length = Number
type Shape = Pt Point
    | Circle Point Length
    | Rect Point Length Length
type alias Figure = List Shape
type alias BBox = (Point,Point)

g : Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h : Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

ntv1 : Bag Int
ntv1 = [(4,1),(7,1),(5,1),(3,1),(8,2)]

ntv2 : Bag Int
ntv2 = [(8,4),(6,3),(7,1),(9,2),(9,1)]

ntv3 : List Int
ntv3 = [7,3,8,7,3,2,7,5]

-- Exercise 1
--
-- (a)Define the function ins that inserts an element into a multiset
ins : a -> Bag a -> Bag a
ins elem bagF = case bagF of
    [] -> [(elem, 1)]
    x::xs ->  if fst x == elem
        then [(elem, (snd x) + 1)] ++ xs 
        else [x] ++ ins elem xs
-- ins 5 ntv1


-- (b)Define the function del that removes a single element from a multiset.
-- Note that deleting 3 from {2,3,3,4} yields {2,3,4} whereas deleting 3 from
-- {2,3,4} yields {2,4}
del : a -> Bag a -> Bag a
del elem bagF = case bagF of
    [] -> []
    (x,y)::ys -> if elem==x && y>1 then (x, (y-1))::ys else if elem == x && y ==1 then ys else (x,y)::del elem ys
-- del 4 ntv1

-- (c)Define a function bag that takes a list of values and produces a multiset 
-- representation. For example with xs = [7,3,8,7,3,2,7,5] we get the following result
-- > bag xs
-- [(5,1),(7,3),(2,1),(3,2),(8,1)]
bag : List a -> Bag a
bag l = case l of
    [] -> []
    x::xs -> ins x (bag xs)
-- bag ntv3

-- (d) Define a function subbag that determines whether or not its first argument bag
-- is contained in the second.
member : a -> List a -> Bool
member x xs =
  any (\a -> a == x) xs

subbag : Bag a -> Bag a -> Bool
subbag x y = case x of
    [] -> False
    n::_ -> 
        case y of
        [] -> False
        _::_ -> (member (fst n) (createList y))

createList : Bag a -> List a
createList b = case b of
    [] -> []
    x::xs -> (fst x) :: createList xs
-- subbag ntv1 ntv2

-- (e) Define a function isSet that tests whether a bag is actually a set, which is the
-- case when each element occurs only once.
isSet : Bag a -> Bool
isSet b = case b of
    [] -> True
    (x,y)::ys -> if y == 1 then isSet ys else False
 -- isSet ntv1

-- (f) Define a function size that computes the number of elements contained in a bag
size : Bag a -> Int
size b = case b of
    [] -> 0
    (_,y)::xs -> y + size xs
-- size ntv1

-- Exercise 2
--
-- (a) Define the function nodes : Graph -> List Node that computes the list of nodes
-- contained in a given graph. For example, nodes g = [1,2,3,4]
tupleMember : a -> List (a,b) -> Bool
tupleMember i l = case l of 
    [] -> False
    (x,y)::ys -> i==x || tupleMember i ys

nodes : Graph -> List Node
nodes r = case r of
    [] -> []
    (x,y)::xs -> if tupleMember x xs then nodes xs else x::nodes xs
-- nodes g

-- (b) Define the function suc : Node -> Graph -> List Node that computes the list of
-- sucessors for a node in a given graph. For example, suc 2 g = [3,4], suc 4 g = [], and
-- suc 4 h = [4]
suc a b = case b of 
    [] -> []
    x::xs -> if (fst x) == a then (snd x)::[] ++ suc a xs else suc a xs
-- suc 2 g

-- (c) Define the function detach : Node -> Graph -> Graph that removes a node together with
-- all of its incident edges from a graph. For example detach 3 g = [(1,2),(2,4)] and 
-- detach 2 h = [(1,3),(4,4)].
detach : Node -> Graph -> Graph
detach n l = case l of 
    [] -> []
    (x,y)::ys -> if n == x || n == y
        then detach n ys 
        else (x,y)::detach n ys
-- detach 3 g

-- -- Exercise 3
-- --
-- -- (a) Define the function width that computes the width of a shape.
width : Shape -> Length
width s = case s of
    Pt _-> 0
    Circle _ b -> b*2
    Rect _ c _-> c
--For example, the widths of the shapes in the figure f are as follows.
f : Figure
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
-- map width f

-- (b) Define the function bbox that computes the bounding box of a shape.
bbox : Shape -> BBox
bbox s = case s of 
    Pt a -> ((fst a,snd a), (fst a,snd a) )
    Circle b c ->(((fst b) - c,(snd b) - c), ((fst b) +c ,  (snd b) + c))
    Rect d e gg-> (((fst d) ,(snd d)), ((fst d) + e, (snd d) + gg))
-- map bbox f

-- (c) Define the function minX that computes the minimum x coordinate of a shape.
minX : Shape -> Number
minX s = case s of 
    Pt a -> min (fst a) (snd a)
    Circle b c ->  min (min ((fst b) - c) ((snd b) - c)) (min ((fst b) + c) ((snd b) + c)) 
    Rect d e gg-> min (min (fst d) (snd d)) (min ((fst d) + e) ((snd d) + gg)) 
-- map minX f

-- (d) Define a function move that moves the position of a shape by a vector given by a point as its first argument.
addPoint : Point -> Point -> Point
addPoint p1 p2 = ((fst p1)+(fst p2), (snd p1)+(snd p2))

move : Point -> Shape -> Shape
move p s = case s of
    Pt a -> Pt (addPoint p a) 
    Circle b c -> Circle (addPoint p b) (c)
    Rect d e gg-> Rect (addPoint p d) (e) (gg)
-- move(5,5) (Pt(1,8))
-- move(5,5) (Circle(1,8) 5)
-- move(5,5) (Rect(1,8) 5 6)