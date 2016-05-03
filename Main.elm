module Main exposing (..)

import Html
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Piece.Model as Piece
import Piece.Update as Piece
import Piece.View as Piece
import Piece.Types as Piece


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { piece : Piece.Model
  }


init : ( Model, Cmd a )
init =
  let
    ( piece, cmd ) =
      Piece.init (Piece.Position 100 100)
  in
    ( { piece = piece }, cmd )


type Msg
  = PieceMsg Piece.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PieceMsg pieceMsg ->
      let
        ( piece', cmd ) =
          Piece.update pieceMsg model.piece
      in
        ( { model | piece = piece' }, cmd |> Cmd.map PieceMsg )


subscriptions : Model -> Sub Msg
subscriptions model =
  Piece.subscriptions model.piece |> Sub.map PieceMsg



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
    [ width "600", height "600" ]
    [ Piece.view model.piece |> Html.map PieceMsg
    ]
