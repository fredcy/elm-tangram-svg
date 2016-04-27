module Main exposing (..)

import Html
import Html.App as Html
import Html.Attributes as Html
import Html.Events as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (always Sub.none)
    }


type alias Model =
  { x : Int
  , y : Int
  }


init : ( Model, Cmd a )
init =
  ( Model 10 10, Cmd.none )


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


roundRect : Model -> Svg Msg
roundRect model =
  rect
    [ x <| toString model.x
    , y <| toString model.y
    , width "100"
    , height "100"
    , rx "15"
    , ry "15"
    , onClick <| Click "roundRect"
    , onMouseDown <| MouseDown
    , onMouseUp <| MouseUp
    , onMouseMove <| MouseMove
    ]
    []


theCircle =
  circle
    [ cx "60"
    , cy "160"
    , r "40"
    , fill "red"
    , onClick <| Click "theCircle"
    ]
    []


type Msg
  = Click String
  | MouseDown
  | MouseUp
  | MouseMove


update : Msg -> b -> ( b, Cmd c )
update msg model =
  case msg |> Debug.log "msg" of
    _ ->
      ( model, Cmd.none )
