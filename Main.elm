module Main exposing (..)

import Html
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
--import Svg.Lazy
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


type alias Name =
  String


type alias Model =
  { pieces : List ( Name, Piece.Model )
  }


init : ( Model, Cmd a )
init =
  let
    pieces =
      [ ( "one", (Piece.init (Piece.Position 100 100)) )
      , ( "two", (Piece.init (Piece.Position 200 200)) )
      ]
  in
    ( { pieces = pieces }, Cmd.none )


type Msg
  = PieceMsg Name Piece.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg |> Debug.log "msg" of
    PieceMsg name pieceMsg ->
      let
        ( pieces', cmds ) =
          updatePieces name pieceMsg model.pieces
      in
        ( { model | pieces = pieces' }, Cmd.batch cmds )


updatePieces : Name -> Piece.Msg -> List ( Name, Piece.Model ) -> ( List ( Name, Piece.Model ), List (Cmd Msg) )
updatePieces name msg items =
  let
    updatePiece (( pieceName, piece ) as item) ( items, cmds ) =
      if pieceName == name then
        let
          ( piece', cmd ) =
            Piece.update msg piece
        in
          ( ( pieceName, piece' ) :: items, Cmd.map (PieceMsg name) cmd :: cmds )
      else
        ( item :: items, cmds )
  in
    List.foldl updatePiece ( [], [] ) items


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    mapSubs ( name, piece ) =
      Piece.subscriptions piece |> Sub.map (PieceMsg name)
  in
    List.map mapSubs model.pieces |> Sub.batch



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
    (List.map pieceView model.pieces)


pieceView : ( Name, Piece.Model ) -> Svg.Svg Msg
pieceView ( name, piece ) =
  Piece.view piece |> Html.map (PieceMsg name)
