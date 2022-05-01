module HW3_Semantics exposing (..)
----------------------------------------
-- CS 381 HW3 Contributors
----------------------------------------
-- SPRING 2022
----------------------------------------
-- Fatimah Alsalman  
-- Travis Hudson 
-- Manju Kuah
-- David Kwak
----------------------------------------

-- Exercise 1. A Stack Language
--
-- Consider the stack language defined by the following grammar
--              prog ::= op | op ; prog
--              op   ::= LD num | ADD | MULT | DUP
--
-- a stack program essentially consists of a (non-empty) sequence of operations, as given by the nonterminal op. The
-- meaning of a stack program is to start with an empty stack and to perform its first operation on it, which results in a
-- new stack to which the next operation in is then applied, and so on. The stack that results from the application of
-- the last operation is the result of the program.
--
-- The operation LD loads its integer parameter onto the stack. The operation ADD removes the two top most integers
-- from the stack and puts their sum onto the stack. If the stack contains fewer than two elements, ADD produces an
-- error. Similarly, the operation MULT takes the two topmost integers from the stack and puts their product on top of
-- the stack. It also produces an error if the stack contains fewer than two elements. Finally, the operation DUP places
-- a second copy of the stack’s topmost element on the stack. (You can find out the error condition for DUP yourself.)
-- Here is a definition of the abstract syntax that you should use.
--              type Op = LD Int | ADD | MULT | DUP
--              type alias Prog = List Op
--
-- Integer stacks should be represented by the type List Int,that is,your program should contain and use the following
-- definition.
--              type alias Stack = List Int
--
type Op = LD Int | ADD | MULT | DUP
type alias Prog = List Op
type alias Stack = List Int

-- Define the semantics for the stack language as an Elm function semProg that yields the semantics of a stack program.
-- Note that the semantic domain has to be defined as a function domain (since the meaning of a stack program is a
-- transformation of stacks) and as an error domain (since operations can fail). Therefore, semProg has the following
-- type where you have to find an appropriate type definition for D.
--              type (alias) D = ...
--              semProg : Prog -> D
--
type alias D = Stack -> Stack
semProg : Prog -> D
semProg prog s = case prog of
    [] -> s
    x::xs -> semProg xs (semOp x s)
    --_ -> Nothing

-- semProg [LD 4] [5]
-- [4,5]
-- 
-- semProg [DUP] [5,6]
-- [5,5,6]
-- DUP :: [] -> semOp x s

-- semProg [DUP, ADD] [5,6]
-- DUP :: [ADD] -> semOp x s
-- s = [5,5,6]
--

-- semProg [LD 3,DUP,ADD,DUP,MULT] [5]
-- [36,5]

-- As support for the definition of semProg you should define an auxiliary function semCmd for the semantics of individual
-- operations, which has the following type.
--              semOp : Op -> D
--
semOp : Op -> D
semOp op s = case op of
    LD i -> i :: s
--          semOp (LD 4) [5]
--          [4,5]
    ADD  -> case s of
        []    -> []
        x::y::xs -> List.sum[x, y] :: xs
        _ ->  []
--          semOp ADD [5,6]
--          [11]
--          semOp ADD [5,6,7]
--          [11,7]
    MULT  -> case s of
        []    -> []
        x::y::xs -> List.product[x, y] :: xs
        _ -> []
--          semOp MULT [5,6]
--          [30]
--          semOp MULT [5,6,7]
--          [30,7]
    DUP  -> case s of
        []    -> []
        x::xs -> [x, x] ++ xs
--          semOp DUP [5,6]
--          [5,5,6]

-- Hint: Test your definitions with the stack programs [LD 3,DUP,ADD,DUP,MULT] and [LD 3,ADD] and the empty stack
-- [] as inputs.