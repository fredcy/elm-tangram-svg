module Piece.Update exposing (..)

import Piece.Types exposing (..)
import Piece.Model exposing (Model, getPosition, getRotation)


update : Context -> Msg -> Model -> ( Model, Cmd c )
update context msg model =
  let
    model' =
      updateHelp context msg model
  in
    ( model', Cmd.none )


updateHelp : Context -> Msg -> Model -> Model
updateHelp context msg model =
  case msg of
    DragStart xy ->
      if context.shift then
        { model | drag = Just (Rotating { start = xy, current = xy }) }
      else
        { model | drag = Just (Dragging { start = xy, current = xy }) }

    DragAt xy ->
      let
        drag =
          case model.drag of
            Nothing ->
              Nothing |> Debug.log "bogus drag when not dragging?"

            Just (Dragging { start }) ->
              Just (Dragging { start = start, current = xy })

            Just (Rotating { start }) ->
              Just (Rotating { start = start, current = xy })
      in
        { model | drag = drag }

    DragEnd xy ->
      { model
        | position = getPosition model
        , rotation = getRotation model
        , drag = Nothing
      }
