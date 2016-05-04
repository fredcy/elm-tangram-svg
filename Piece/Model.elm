module Piece.Model exposing (..)

import Mouse
import Piece.Types exposing (..)
import Svg exposing (Svg)


type alias Model =
  { shape : Shape
  , position : Position
  , rotation : Rotation
  , drag : Maybe Drag
  }


init : Shape -> Position -> Rotation -> Model
init shape position rotation =
  Model shape position rotation Nothing


getPosition : Model -> Position
getPosition { position, drag } =
  case drag of
    Nothing ->
      position

    Just (Dragging { start, current }) ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

    Just DragStarting ->
      position


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Mouse.downs MouseDown

    Just _ ->
      Sub.batch [ Mouse.downs MouseDown, Mouse.moves DragAt, Mouse.ups DragEnd ]
