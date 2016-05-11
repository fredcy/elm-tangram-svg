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
        { model | drag = Just (Rotating { start = xy, current = xy, sample = Nothing }) }
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

            Just (Rotating state) ->
              let
                sample =
                  if state.sample == Nothing && distance state.start xy > 20 then
                    Just xy
                  else
                    state.sample
              in
                Just (Rotating { state | current = xy, sample = sample })
      in
        { model | drag = drag }

    DragEnd xy ->
      { model
        | position = getPosition model |> restrictTo context.size
        , rotation = getRotation model
        , drag = Nothing
      }


distance : Position -> Position -> Float
distance p1 p2 =
  sqrt (toFloat (p1.x - p2.x) ^ 2 + toFloat (p1.y - p2.y) ^ 2)
       

restrictTo : { width : Int, height: Int } -> Position -> Position
restrictTo { width, height } { x, y } =
  { x = clamp 0 width x
  , y = clamp 0 height y
  }
