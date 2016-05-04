module Piece.Update exposing (..)

import Piece.Types exposing (..)
import Piece.Model exposing (Model, getPosition)


update : Msg -> Model -> ( Model, Cmd c )
update msg model =
  let
    model' =
      updateHelp msg model
  in
    ( model', Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({ position, drag } as model) =
  case msg of
    DragStart xy ->
      { model | drag = Just (Dragging { start = xy, current = xy }) }

    DragAt xy ->
      let
        drag =
          case model.drag of
            Nothing ->
              Nothing |> Debug.log "bogus"

            Just (Dragging { start }) ->
              Just (Dragging { start = start, current = xy })
      in
        { model | drag = drag }

    DragEnd _ ->
      { model | position = getPosition model, drag = Nothing }
