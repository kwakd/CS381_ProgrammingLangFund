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
--              rankOp : Op -> OpRank
--
rankOp : Op -> OpRank
rankOp op = 

-- Then define a function rankP that computes the rank of a program. The Maybe data type is used to capture rank
-- errors, that is, a program that contains a rank error should be mapped to Nothing whereas ranks of other programs
-- are wrapped by the Just constructor.
--              rankP : Prog -> Maybe Rank
--
-- Hint. Maybe define an auxiliary function rank : Prog -> Rank -> Maybe Rank and define rankP using rank.
--
-- b)  Following the example of the function evalTC (defined on slide 33 of the type system slides), define a function
-- semTC for evaluating stack programs that first calls the function rankP to check whether the stack program is
-- type correct and evaluates the program only in that case. For performing the actual evaluation semTC calls the
-- function semProg (as defined in the previous homework; see the file HW3_Semantics.elm).
-- However, the function semProg called by semTC can be simplified, especially, its type. What is the new type of
-- semProg, and why is it safe to use this new type? (You do not have to give a new definition of the function; you
-- can use the definition semOp _ _ = Debug.todo "Error" to get your file to compile.)


