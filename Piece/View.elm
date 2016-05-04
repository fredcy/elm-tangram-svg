module Piece.View exposing (view)

import Json.Decode as Json
import Mouse
import Piece.Model exposing (Model, getPosition)
import Piece.Types exposing (Msg(DragStart), Shape(..))
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import VirtualDom


view : Model -> Svg Msg
view model =
  let
    realPosition =
      getPosition model
  in
    Svg.node
      "svg"
      [ x (toString realPosition.x)
      , y (toString realPosition.y)
      , VirtualDom.on "mousedown" (Json.map DragStart Mouse.position)
      ]
      [ case model.shape of
          Triangle color scale ->
            triangle color scale model.rotation
      ]


triangle : String -> Float -> Float -> Svg Msg
triangle color scale rotation =
  Svg.polygon
    [ points "0, 0 1,0 0,1"
    , transform ("scale(" ++ toString scale ++ ") rotate(" ++ toString rotation ++ " 0.5 0.5)")
    , fill color
    , stroke "gray"
    , strokeWidth (toString (4.0 / scale))
    ]
    []
