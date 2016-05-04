module Piece.Types exposing (..)


type alias Position =
  { x : Int, y : Int }


type Drag
  = Dragging
      { start : Position
      , current : Position
      }


type Msg
  = DragStart Position
  | DragAt Position
  | DragEnd Position


type alias Color =
  String


type alias Scale =
  Float


type alias Rotation =
  Float


type Shape
  = Triangle Color Scale
  | Square Color Scale
