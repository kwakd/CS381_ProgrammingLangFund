module HW2_Syntax exposing (..)

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

-- Exercise 1 Mini Logo
--
-- cmd ::= pen mode
--      |  moveto (pos,pos)
--      |  def name (pars) cmd
--      |  call name (vals)
--      |  cmd;cmd
--
-- (a)Define the abstract syntax for Mini Logo as Elm types



-- (b) Write a Mini Logo macro vector that draws a line from a given position (x1, y1) to a given position (x2, y2) 
-- and represent the macro in abstract syntax, that is, as an elm data type value


-- (c) Define an Elm function steps : Int -> Cmd that constructs a Mini Logo program which draws a star of n steps
-- Your solution should not use the macro vector


-- Exercise 2 Grammar Grammar
--
-- (a)  Give Elm type (alias) definitions for the types Grammar, Prod, RHS, and Symbol to represent the abstract syntax
-- for the above language. As part of your definitions, use the following type aliases NonTerm and Term.
--     type alias NonTerm = String
--     type alias Term = String

-- (b) Consider the following grammar for a small imperative language Imp that we already encountered in class.
--          cmd ::= T | not cond | (cond)
--          stmt ::= skip | while cond do {stmt} | stmt;stmt
-- Represent this grammar by an Elm value of type Grammar defined in part(a)
--          imp : Grammar
--          imp = ...


-- (c) Define the following two functions for extracting all defined nonterminals and all used terminals in a grammar.
--          nonterminals : Grammar -> List NonTerm
--          terminals : Grammar -> List Term
-- For the value imp defined in part(b), the functions would produce the following results
--          > nonterminals imp
--          ["cond", "stmt"]
--
--          > terminals imp
--          ["T","not","(",")","skip","while","do","{","}",";"]
