module HW4_Types exposing (..)
----------------------------------------
-- CS 381 HW4 Contributors
----------------------------------------
-- SPRING 2022
----------------------------------------
-- Fatimah Alsalman  
-- Travis Hudson 
-- Manju Kuah
-- David Kwak
----------------------------------------

-- Exercise 1. A Rank-Based Type Systems for the Stack Language
--
-- We extend the simple stack language from Homework 3 (Exercise 1) by the following three operations.
--              DEC decrements the topmost element on the stack
--              SWAP exchanges the two topmost elements on the stack, and
--              POP k pops k elements of the stack
--
-- The abstract syntax of this extended language is as follows.
--              type Op = LD Int | ADD | MULT | DUP | DEC | SWAP | POP Int
--              type alias Prog = List Op
--
type Op = LD Int | ADD | MULT | DUP | DEC | SWAP | POP Int
type alias Prog = List Op
type alias Stack = List Int

type alias D = Stack -> Stack

semProg : Prog -> D
semProg prog s = case prog of
    [] -> s
    x::xs -> semProg xs (semOp x s)

semOp : Op -> D
semOp op s = case op of
    LD i -> i :: s
--          semOp (LD 4) [5]
--          [4,5]
    ADD  -> case s of
        x::y::xs -> List.sum[x, y] :: xs
        _ ->  []
--          semOp ADD [5,6]
--          [11]
--          semOp ADD [5,6,7]
--          [11,7]
    MULT  -> case s of
        x::y::xs -> List.product[x, y] :: xs
        _ -> []
--          semOp MULT [5,6]
--          [30]
--          semOp MULT [5,6,7]
--          [30,7]
    DUP  -> case s of
        x::xs -> [x, x] ++ xs
        _ -> []

    DEC  -> case s of
        x::xs -> [x-1] ++ xs
        _ -> []

    SWAP  -> case s of
        x::y::xs -> [y, x] ++ xs
        _ -> []

    POP i -> List.drop i s

-- Even though the stack carries only integers, a type system can be defined for this language that assigns ranks to stacks
-- and operations and ensures that a program does not result in a rank mismatch.
--
-- The rank of a stack is given by the number of its elements. The rank of a stack operation is given by a pair of
-- numbers (n, m) where n is the number of elements taken from the top of the stack and m is number of elements put
-- onto the stack. The rank for a stack program is defined to be the rank of the stack that would be obtained if the
-- program were run on an empty stack. A rank error occurs in a stack program when an operation with rank (n, m) is
-- executed on a stack with rank k < n. In other words, a rank error indicates a stack underflow.
--
-- a) Use the following types to represent stack and operation ranks.
--              type alias Rank = Int
--              type alias OpRank = (Int,Int)
-- 
type alias Rank = Int
type alias OpRank = (Int, Int)

--  First, define a function rankC that maps each stack operation to its rank.
--              rankC : Op -> OpRank
--
rankC : Op -> OpRank
rankC op = case op of
    LD i -> (0,1)
    ADD -> (2,1)
    MULT -> (2,1)
    DUP -> (1,2)
    DEC -> (1,1)
    SWAP -> (2,2)
    POP i -> (i, 0)

-- Then define a function rankP that computes the rank of a program. The Maybe data type is used to capture rank
-- errors, that is, a program that contains a rank error should be mapped to Nothing whereas ranks of other programs
-- are wrapped by the Just constructor.
--              rankP : Prog -> Maybe Rank
--
-- Hint. Maybe define an auxiliary function rank : Prog -> Rank -> Maybe Rank and define rankP using rank.
--
rank : Prog -> Rank -> Maybe Rank
rank prog r = case prog of
    [] -> if r >= 0 then Just r
        else Nothing

    x::xs -> 
        let
            (subs, adds) = rankC x
            under = r - subs
        in 
            if under >= 0 then rank xs (under + adds)
            else Nothing

rankP : Prog -> Maybe Rank
rankP xs = rank xs 0


-- b)  Following the example of the function evalTC (defined on slide 33 of the type system slides), define a function
-- semTC for evaluating stack programs that first calls the function rankP to check whether the stack program is
-- type correct and evaluates the program only in that case. For performing the actual evaluation semTC calls the
-- function semProg (as defined in the previous homework; see the file HW3_Semantics.elm).
-- However, the function semProg called by semTC can be simplified, especially, its type. What is the new type of
-- semProg, and why is it safe to use this new type? (You do not have to give a new definition of the function; you
-- can use the definition semOp _ _ = Debug.todo "Error" to get your file to compile.)

semTC : Prog -> Maybe Stack
semTC prog = if (rankP prog) /= Nothing then Just (semProg prog ([]))
    else Nothing
-- semTC 1bTest_

bTest1 = [LD 5, LD 2] -- Just [2,5]
bTest2 = [LD 5, LD 2, ADD] -- Just [7]
bTest3 = [LD 5, LD 2, MULT] -- Just [10]
bTest4 = [LD 5, LD 2, DUP] -- Just [2, 2, 5]
bTest5 = [LD 5, LD 2, DEC] -- Just [1,5]
bTest6 = [LD 5, LD 2, SWAP] -- Just [5,2]
bTest7 = [LD 5, LD 2, POP 1] -- Just [5]
bTest8 = [LD 5, LD 2, POP 2] -- Just []

bTest9 = [LD 5, DUP, ADD, LD 2, SWAP] -- Just [10, 2]
bTest10 = [LD 5, POP 1, LD 4, DUP, POP 2, LD 2] -- Just [2]

bTest11 = [POP 1] -- NOTHING
bTest12 = [DUP] -- NOTHING

-- Exercise 2. A Rank-Based Type Systems for the Stack Language
-- Consider the following language for building shapes out of unit squares through horizontal and vertical composition.
--          s ∈ shape ::=  | s<s | s^s
-- In this grammar,  denotes a unit square (or pixel) of width and height 1. The expression s1<s2 puts s2 next to s1 (on
-- the right), which means to join s2’s left border with s1’s right border while aligning their bottom sides. The expression
-- s3^s4 puts s3 above s4, which means to join s3’s lower border with s4’s upper border while aligning their left sides.
-- The borders of any shape are given by the smallest enclosing rectangle, which is also called the shape’s bounding
-- box; this means the lower border is given by the lowest contained square(s), the left border is given by the leftmost
-- square(s), and so on. The effect of the operations is illustrated below.
--
-- Here, shape 4 is given by , the shapes 1 and 3 are given by <, and shape 2 is given by ^(^). The left shape is
-- given, for example, by (<)<(^(^)), and the right shape is given by (<)^. We can observe that in many
-- cases the same shape can be built in different ways. For example, shape 2 can also be given by (^)^.
--
-- The width and height of a shape’s bounding box can be considered its type. For example, the type of shapes 1
-- and 3 are both (2, 1), the type of shape 2 is (1, 3), and the type of shape 4 is (1, 1). The type of the following shape
-- is (3, 2).
--
-- The abstract syntax of the shape language is given by the following Elm type.
--         type Shape = X | TD Shape Shape | LR Shape Shape
-- We define the type of a shape to be the pair of integers giving the width and height of its bounding box.
--          type alias BBox = (Int,Int)
--
type Shape = X | TD Shape Shape | LR Shape Shape 
type alias BBox = (Int, Int)

-- A type can be understood as a characterization of values, summarizing a set of values at a higher level, abstracting
-- away from some details and mapping value properties to a coarser description on the type level. In this sense, a
-- bounding box can be considered as a type of shapes. The bounding box classifies shapes into different bounding box
-- types. (The bounding box type information could be used to restrict, for example, the composition of shapes, such as
-- applying TD only to shapes of the same width, although we won’t pursue this idea any further in this exercise.)
--
-- a) Define a type checker for the shape language as an Elm function with the following type
--        bbox : Shape -> BBox
--
bbox : Shape -> BBox
bbox shape = case shape of
    X -> (1,1)

    TD s1 s2 ->
        let
            (s1x, s1y) = bbox s1
            (s2x, s2y) = bbox s2
        in
            if s1x >= s2x then (s1x, s1y + s2y)
            else (s2x, s1y + s2y)

    LR s1 s2 -> 
        let
            (s1x, s1y) = bbox s1
            (s2x, s2y) = bbox s2
        in
            if s1y >= s2y then (s1x + s2x, s1y)
            else (s1x + s2x, s2y)

-- b) Rectangles are a subset of shapes and thus describe a more restricted set of types. By restricting the application
-- of the TD and LR operations to rectangles only one could ensure that only convex shapes without holes can
-- be constructed. Define a type checker for the shape language that assigns types only to rectangular shapes by
-- defining the following Elm function.
--          rect : Shape -> Maybe BBox

{--
rect : Shape -> Maybe BBox
rect shape = case Just(shape) of
    Just X -> Just (1,1)

    Just (TD s1 s2) -> 
        let
            (Just(s1x, s1y)) = (rect s1)
            (Just(s2x, s2y)) = (rect s2)
        in
            if Just s1x == Just s2x then Just(s1x, s1y + s2y)
            else Nothing

    _ -> Nothing

    -- LR s1 s2 -> 
    --     let
    --         (Just(s1x, s1y)) = (rect s1)
    --         (Just(s2x, s2y)) = (rect s2)
    --     in
    --         if Just s1y == Just s2y then Just(s1x + s2x, s1y)
    --         else Nothing
 --}
 
rect: Shape -> Maybe BBox
rect s = case s of
    X -> Just (1,1)
    TD s1 s2 ->
        case (rect s1) of
            Nothing -> Nothing
            Just (s1x, s1y) -> case (rect s2) of
                            Nothing -> Nothing
                            Just (s2x, s2y) -> if s1x == s2x then Just (s1x , s1y + s2y)
                                else Nothing
    LR s1 s2 -> 
        case (rect s1) of
            Nothing -> Nothing
            Just (s1x, s1y) -> case (rect s2) of
                            Nothing -> Nothing
                            Just (s2x, s2y) -> if s1y == s2y then Just (s1x + s2x , s1y)
                                else Nothing



