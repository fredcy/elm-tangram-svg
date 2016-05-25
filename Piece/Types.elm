module Piece.Types exposing (..)

import Colors exposing (Color)
import Window


type alias Position =
    { x : Int, y : Int }


type Drag
    = Dragging
        { start : Position
        , current : Position
        }
    | Rotating
        { start : Position
        , current : Position
        , sample : Maybe Position
        }


type Msg
    = DragStart ( Position, Position )
    | DragAt Position
    | DragEnd Position


type alias Scale =
    Float


type alias Rotation =
    Float


type Shape
    = Triangle Color Scale
    | Square Color Scale
    | Parallelogram Color Scale


type alias Context =
    { shift : Bool
    , size : Window.Size
    }


distance : Position -> Position -> Float
distance p1 p2 =
    sqrt (toFloat (p1.x - p2.x) ^ 2 + toFloat (p1.y - p2.y) ^ 2)
