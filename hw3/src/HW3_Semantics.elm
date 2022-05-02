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

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

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

-- Elm/SVG auxiliary definitions
--
scale = 30

xPt : Int -> String
xPt i = String.fromInt (scale+i*scale)

yPt : Int -> String
yPt i = String.fromInt (398-i*scale)

svgLine : Line -> Svg a
svgLine ((a,b),(c,d)) = line
    [ x1 (xPt a), y1 (yPt b), x2 (xPt c), y2 (yPt d)
    , stroke "green", strokeWidth "4", strokeLinecap "round"] []

main : Html msg
main = svg [viewBox "0 0 400 400", width "800", height "800"]
           (List.map svgLine logoResult)

----- BEGIN HW3 solution

type alias Point = (Int,Int)

type Cmd = Pen Mode
         | MoveTo Point
         | Seq Cmd Cmd

type Mode = Up | Down

type alias Line = (Point,Point)
type alias Lines = List Line
type alias State = (Mode,Point)

head = List.head

semCmd : Cmd -> State -> (State,Lines)
semCmd c (m, (x,y)) = case c of 
           {-- Pen Up -> ((Up, (x,y)), [])
            Pen Down -> ((Down, (x,y)), [])
            MoveTo (j, k) -> ((m, (j,k)), [((x,y),(j,k))])
            --Seq c1 c2 -> 
            --}
            Pen Up   ->  ((Up, (x,y)), [] )
            Pen Down -> ((Down, (x,y)), [] )
            MoveTo (e1, e2) -> case (m,(x,y)) of
                (Up, (f1, f2)) -> ((Up, (e1,e2)), [])
                (Down, (f1, f2)) -> ((Down, (e1,e2)), [ ((f1,f2),(e1,e2)) ] ++ [])
            Seq c1 c2 -> let 
                            (n1, m1) = (semCmd c1 (m,(x,y)))
                        in 
                        let (n2, m2) = semCmd c2 n1 in (n2, m1++m2 )
            {--Seq c1 c2 -> let 
                            state2 = (semCmd c1 (m,(x,y)))
                        in 
                        semCmd c2 ((Tuple.first( state2)))--}

lines : Cmd -> Lines
lines c = case c of
        Pen _ -> []
        MoveTo (_, _) -> []
        Seq c1 c2 -> let (_, n1) = semCmd (Seq c1 c2) (Up, (0,0)) in n1
       -- cmd -> return_points(semCmd cmd (Up, (0,0)))::[]
       --cmd -> return_points(semCmd cmd (Up, (0,0)))++[]

return_points : (State, Lines) -> Lines--Line
return_points ((m , (x,y)), l) = case l of
                ls -> ls
{--return_points ((m , (x,y)), l) = case (x,y) of 
        (j ,k) ->((x,y),(j,k))--}

logoResult : Lines
logoResult = lines (Seq (Seq (Seq (Pen Up) (Seq (MoveTo (0,0)) (Seq (Pen Down) (MoveTo (0,1))))) (MoveTo (1,1))) (Seq (MoveTo (1,2)) (MoveTo (2,2))))
--logoResult = [((0,0),(0,1)), ((0,1),(1,1)), ((1,1),(1,2)), ((1,2),(2,2))]
