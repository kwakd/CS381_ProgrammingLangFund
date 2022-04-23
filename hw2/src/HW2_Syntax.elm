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
-- mode ::= up  | down
-- pos  ::= num | name
-- pars ::= num, pars | name
-- vals ::= num, vals | num

-- *****************************************************************
-- (a) Define the abstract syntax for Mini Logo as Elm types
type Cmd = Pen Mode
        | MoveTo Pos Pos
        | Def String Pars Cmd
        | Call String Vals
        | Seq Cmd Cmd

type Mode = Up | Down 
type Pos = IPo Int | SPo String
type Pars = IPa (List Int) Pars | SPa String
type Vals = IV1 (List Int) Vals | IV2 Int

-- *****************************************************************

-- *****************************************************************
-- (b) Write a Mini Logo macro vector that draws a line from a given position (x1, y1) to a given position (x2, y2) 
-- and represent the macro in abstract syntax, that is, as an elm data type value
-- 
--      Note: What you should actually do is write a Mini Log program that defines a vector macro. Using concrete syntax, the answer would have the following form
--              def vector (...) ...
--
--      It might be a good idea to write the solution in concrete syntax first. But then you should write the same Mini Logo program in abstract syntax, that is,
--      you should define a value built with Elm constructors that starts as follows (assuming Def is the construct that represents the def production in the Elm type used for cmd)
--             vector = Def "vector" ... ...
--
--      You only need to submit this Elm definition of the value vector as part of your Elm program. (If you like, you can include the concret syntax as a comment, but it is not required)

-- Concrete Syntax
-- def vector (x1, y1, x2, y2) (pen down, MoveTo(x1,y1), MoveTo(x2,y2), pen up)

-- Abstract Syntax
vector = Def "vector" (IPa[5](SPa "")) (Seq(Seq(Pen Down)(MoveTo (IPo 5) (IPo 6)))(Seq(Pen Down)(MoveTo (IPo 5) (IPo 6))))

-- *****************************************************************

-- *****************************************************************
-- (c) Define an Elm function steps : Int -> Cmd that constructs a Mini Logo program which draws a star of n steps
-- Your solution should not use the macro vector
--steps : Int -> Cmd
--steps userInput = case userInput of
 --   0 -> _ 



-- *****************************************************************

-- Exercise 2 Grammar Grammar
--
-- Consider the following grammar that describes the syntax of the language for grammar definitions
--          grammar ::= prod; ...; prod
--          prod    ::= nt ::= rhs | ... | rhs
--          rhs     ::= symbol*
--          symbol  ::= nt | term
-- A grammar is given by a list of (grouped) productions (prod), each of which consists of a nonterminal nt and a list of 
-- alternative right-hand sides(rhs). A right-hand side of a production is given by a sequence of terminal(term) and nonterminal(nt) symbols
--
--      Note carefully the difference between the object language symbols ::= and | (typeset in blue typewriter font) and the similair-looking symbols ::=
--      and | that belongs to the grammar metalanguage
--
-- *****************************************************************
-- (a)  Give Elm type (alias) definitions for the types Grammar, Prod, RHS, and Symbol to represent the abstract syntax
-- for the above language. As part of your definitions, use the following type aliases NonTerm and Term.
--     type alias NonTerm = String
--     type alias Term = String
-- *****************************************************************

-- *****************************************************************
-- (b) Consider the following grammar for a small imperative language Imp that we already encountered in class.
--          cmd ::= T | not cond | (cond)
--          stmt ::= skip | while cond do {stmt} | stmt;stmt
-- Represent this grammar by an Elm value of type Grammar defined in part(a)
--          imp : Grammar
--          imp = ...
-- *****************************************************************

-- *****************************************************************
-- (c) Define the following two functions for extracting all defined nonterminals and all used terminals in a grammar.
--          nonterminals : Grammar -> List NonTerm
--          terminals : Grammar -> List Term
-- For the value imp defined in part(b), the functions would produce the following results
--          > nonterminals imp
--          ["cond", "stmt"]
--
--          > terminals imp
--          ["T","not","(",")","skip","while","do","{","}",";"]
-- *****************************************************************