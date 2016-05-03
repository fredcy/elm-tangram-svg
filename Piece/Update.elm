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
    DragStart ->
      { model | drag = Just DragStarting }

    DragAt xy ->
      let
        drag =
          case model.drag of
            Nothing ->
              Nothing |> Debug.log "bogus"

            Just DragStarting ->
              Just (Dragging { start = xy, current = xy }) |> Debug.log "bogus?"

            Just (Dragging { start }) ->
              Just (Dragging { start = start, current = xy })
      in
        { model | drag = drag }

    DragEnd _ ->
      { model | position = getPosition model, drag = Nothing }

    MouseDown pos ->
      let
        -- [naming the following as `drag` causes a runtime crash]
        drag' =
          case drag of
            Just DragStarting ->
              Just (Dragging { start = pos, current = pos })

            _ ->
              drag
      in
        { model | drag = drag' }
