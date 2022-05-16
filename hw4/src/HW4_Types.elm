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
            (subs, adds) = rankC x --(2,1)
            under = r - subs --0
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
