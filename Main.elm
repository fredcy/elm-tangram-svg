module Main exposing (..)

import Html
import Html.App as Html
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode as Json
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Mouse exposing (Position)


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { position : Position
  , drag : Maybe Drag
  }


type Drag
  = DragStarting
  | Dragging
      { start : Position
      , current : Position
      }


init : ( Model, Cmd a )
init =
  ( Model (Position 10 10) Nothing
  , Cmd.none
  )



-- update


type Msg
  = DragStart
  | DragAt Position
  | DragEnd Position
  | MouseDown Position


update : Msg -> Model -> ( Model, Cmd c )
update msg model =
  let
    model' =
      updateHelp msg model |> Debug.log "model"
  in
    ( model', Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({ position, drag } as model) =
  case msg |> Debug.log "msg" of
    DragStart ->
      { model | drag = Just DragStarting }

    DragAt xy ->
      let
        drag =
          case model.drag of
            Nothing ->
              Nothing

            Just DragStarting ->
              Just (Dragging { start = xy, current = xy })

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


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Mouse.downs MouseDown

    Just _ ->
      Sub.batch [ Mouse.downs MouseDown, Mouse.moves DragAt, Mouse.ups DragEnd ]



-- view


view : Model -> Html.Html Msg
view model =
  Html.div
    []
    [ Html.h1 [] [ Html.text "hello!" ]
    , scene model
    ]


scene : Model -> Html.Html Msg
scene model =
  Svg.svg
    [ width "200", height "200" ]
    [ roundRect model
    , theCircle
    ]


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


theCircle =
  circle
    [ cx "60"
    , cy "160"
    , r "40"
    , fill "red"
    ]
    []
