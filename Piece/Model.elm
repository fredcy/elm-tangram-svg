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
    Just (Dragging { start, current }) ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

    _ ->
      position


getRotation : Model -> Rotation
getRotation { rotation, drag } =
  case drag of
    Just (Rotating { start, current }) ->
      let
        angleR = atan2 (toFloat (current.y - start.y)) (toFloat (current.x - start.x))
        angleD = (angleR / (2 * pi)) * 360
      in
        rotation + angleD

    _ ->
      rotation


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
