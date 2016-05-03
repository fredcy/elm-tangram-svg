module Piece.View exposing (..)

import Piece.Model exposing (..)
import Piece.Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


view : Model -> Svg Msg
view model =
  let
    realPosition =
      getPosition model
  in
    Svg.circle
      [ cx (ts realPosition.x)
      , cy (ts realPosition.y)
      , r "40"
      , fill "red"
      , onMouseDown DragStart
      ]
      []


ts : Int -> String
ts i =
  toString i


roundRect : Model -> Svg Msg
roundRect model =
  let
    realPosition =
      getPosition model
  in
    rect
      [ x <| toString realPosition.x
      , y <| toString realPosition.y
      , width "100"
      , height "100"
      , rx "15"
      , ry "15"
      , onMouseDown DragStart
      ]
      []
