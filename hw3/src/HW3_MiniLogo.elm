module HW3_MiniLogo exposing (..)

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

-- Exercise 2. Mini Logo
--
-- Consider the simplified version of Mini Logo (without macros), defined by the following abstract syntax.
--              type alias Point = (Int,Int)
--              type Mode = Up | Down
--
--              type Cmd = Pen Mode
--                      | MoveTo Point
--                      | Seq Cmd Cmd
--
type alias Point = (Int,Int)

type Cmd = Pen Mode
         | MoveTo Point
         | Seq Cmd Cmd

type Mode = Up | Down

-- The semantics of a Mini Logo program is a set of drawn lines. However, for the definition of the semantics a "drawing state"
-- must be maintained that keeps track of the current position of the pen and the pen's status (Up or Down). This state should
-- be represented by values of the following type.
--              type alias State = (Mode,Point)
--
type alias State = (Mode,Point)

-- The semantic domain representing a set of drawn lines is represented by the type Lines.
--              type alias Line = (Point,Point)
--              type alias Lines = List Line
--
type alias Line = (Point,Point)
type alias Lines = List Line

-- Define the semantics of Mini Logo via two Elm functions. First, define a function semCmd that has the following type.
--              semCmd : Cmd -> State -> (State,Lines)
--
semCmd : Cmd -> State -> (State,Lines)
semCmd c(m,(x,y)) = 
    case c of 
        Pen Up   ->  ((Up, (x,y)), [] )
        Pen Down -> ((Down, (x,y)), [] )
        MoveTo (e1, e2) -> case (m,(x,y)) of
            (Up, (f1, f2)) -> ((Up, (f1,f2)), [])
            (Down, (f1, f2)) -> ((Down, (f1,f2)), [] )
        Seq c1 c2 -> let state2 = (semCmd c1 (m,(x,y))) in semCmd c2 (Tuple.first(state2))
        --_ -> ((m,(x,y)), [])
       -- MoveTo (e1, e2)  ->  ((Down, (5,5)), [] )

-- semCmd (Seq (Pen Up)(MoveTo (0,0))) (Up,(4,5)) -> (((Pen Up), (4,5), [(4,5)(4,5)])

-- This function defines for each Cmd how it modifies the current drawing state and what lines it produces. After that
-- define the function lines with the following type.
--              lines : Cmd -> Lines
--
lines : Cmd -> Lines
lines c = Tuple.second(semCmd(c) (Up,(0,0)))


-- lines (Seq (Pen Up)(MoveTo (0,0))) -> [List Line]

-- The function lines should call semCmd. The initial state is defined to have the pen up and the current drawing
-- position at (0,0).

logoResult : Lines
logoResult = lines (Seq (Seq (Seq (Pen Up) (Seq (MoveTo (0,0)) (Seq (Pen Down) (MoveTo (0,1))))) (MoveTo (1,1))) (Seq (MoveTo (1,2)) (MoveTo (2,2))))
