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
            Pen Up   ->  ((Up, (x,y)), [] )
            Pen Down -> ((Down, (x,y)), [] )
            MoveTo (e1, e2) -> case (m,(x,y)) of
                (Up, (f1, f2)) -> ((Up, (e1,e2)), [])
                (Down, (f1, f2)) -> ((Down, (e1,e2)), [ ((f1,f2),(e1,e2)) ] ++ [])
            Seq c1 c2 -> let 
                            (n1, m1) = (semCmd c1 (m,(x,y)))
                        in 
                        let (n2, m2) = semCmd c2 n1 in (n2, m1++m2 )


lines : Cmd -> Lines
lines c = case c of
       cmd -> return_points(semCmd cmd (Up, (0,0)))++[]

return_points : (State, Lines) -> Lines--Line
return_points ((m , (x,y)), l) = case l of
                ls -> ls


logoResult : Lines
logoResult = lines (Seq (Seq (Seq (Pen Up) (Seq (MoveTo (0,0)) (Seq (Pen Down) (MoveTo (0,1))))) (MoveTo (1,1))) (Seq (MoveTo (1,2)) (MoveTo (2,2))))
--logoResult = [((0,0),(0,1)), ((0,1),(1,1)), ((1,1),(1,2)), ((1,2),(2,2))]
