module Main exposing (..)

import Dict exposing (Dict)
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


type alias PieceName =
  String


type alias Model =
  { pieces : Dict PieceName Piece.Model
  }


init : ( Model, Cmd a )
init =
  let
    pieces =
      Dict.singleton "one" (Piece.init (Piece.Position 100 100))
  in
    ( { pieces = pieces }, Cmd.none )


type Msg
  = PieceMsg PieceName Piece.Msg


updatePiece : Piece.Msg -> Maybe Piece -> Maybe Piece
updatePiece msg pieceMaybe =
  Maybe.map (Piece.update pieceMsg) pieceMaybe
       

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PieceMsg pieceName pieceMsg ->
      let
        pieces' = Dict.update pieceName (updatePiece pieceMsg) pieces
      in
        case pieceMaybe of
          Nothing ->
            Debug.crash "nothing for piece"

          Just piece ->
            let
              ( piece', cmd ) = Piece.update pieceMsg piece
              pieces 
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
