module Piece.Types exposing (..)


type alias Position =
  { x : Int, y : Int }


type Drag
  = DragStarting
  | Dragging
      { start : Position
      , current : Position
      }


type Msg
  = DragStart
  | DragAt Position
  | DragEnd Position
  | MouseDown Position


