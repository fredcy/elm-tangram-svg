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
    Svg.node
      "svg"
      [ x (ts realPosition.x)
      , y (ts realPosition.y)
      , onMouseDown DragStart
      ]
      [ Svg.circle
          [ cx "40"
          , cy "40"
          , r "40"
          , fill "red"
          ]
          []
      , Svg.circle
          [ cx "40"
          , cy "40"
          , r "30"
          , fill "black"
          ]
          []
      ]


ts : Int -> String
ts i =
  toString i
