module Piece.View exposing (view)

import Json.Decode as Json
import Mouse
import Piece.Model exposing (Model, getPosition)
import Piece.Types exposing (Msg(DragStart), Shape(..))
import String
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
      [ VirtualDom.on "mousedown" (Json.map DragStart Mouse.position)
      ]
      [ case model.shape of
          Triangle color scale ->
            polygon trianglePoints color scale model.rotation ( toFloat realPosition.x, toFloat realPosition.y )

          Square color scale ->
            polygon squarePoints color scale model.rotation ( toFloat realPosition.x, toFloat realPosition.y )
          
          Parallelogram color scale ->
            polygon paraPoints color scale model.rotation ( toFloat realPosition.x, toFloat realPosition.y )
      ]


trianglePoints =
  [ ( 0, -0.5 ), ( 1, 0.5 ), ( -1, 0.5 ) ]


squarePoints =
  [ ( 0, -0.5 ), ( 0.5, 0 ), ( 0, 0.5 ), ( -0.5, 0 ) ]


paraPoints =
  [ ( 0.25, -0.25 ), ( -0.75, -0.25 ), ( -0.25, 0.25 ), ( 0.75, 0.25 ) ]


polygon : List ( Float, Float ) -> String -> Float -> Float -> ( Float, Float ) -> Svg Msg
polygon shape color scale rotation position =
  let
    vertices =
      shape |> List.map (scalePoint scale >> rotatePoint (degrees rotation) >> translatePoint position)
  in
    Svg.polygon
      [ points <| pointsToString vertices
      , fill color
      , stroke "gray"
      , strokeWidth (toString (8))
      , strokeLinejoin "round"
      ]
      []


rotatePoint : Float -> ( Float, Float ) -> ( Float, Float )
rotatePoint angle ( x, y ) =
  let
    x' =
      x * cos angle - y * sin angle

    y' =
      x * sin angle + y * cos angle
  in
    ( x', y' )


scalePoint : Float -> ( Float, Float ) -> ( Float, Float )
scalePoint factor ( x, y ) =
  ( x * factor, y * factor )


translatePoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
translatePoint ( dx, dy ) ( x, y ) =
  ( x + dx, y + dy )


pointsToString : List ( Float, Float ) -> String
pointsToString list =
  List.map (\( x, y ) -> toString x ++ " " ++ toString y) list |> String.join " "
