module Main exposing (..)

import Html
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import VirtualDom


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
  , height : Int
  , width : Int
  }


init : ( Model, Cmd a )
init =
  let
    pieces =
      [ ( "one", (Piece.init (Piece.Triangle "red" 100.0) (Piece.Position 100 100) 0) )
      , ( "two", (Piece.init (Piece.Triangle "orange" 100.0) (Piece.Position 200 200) 45) )
      ]
  in
    ( { pieces = pieces, height = 600, width = 600 }, Cmd.none )


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


{-| Fold over the list of components and apply the msg to the component piece
with the matching name, collecting the updated models and resulting commands
into separate lists as needed for updating the main model and batching the
commands.
-}
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
    List.foldr updatePiece ( [], [] ) items


{-| Gather the subscriptions of each of the components into a single Batch.
-}
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
    [ Html.h1 [] [ Html.text "SVG drag and drop" ]
    , scene model
    , debugInfo model
    ]


scene : Model -> Html.Html Msg
scene model =
  Svg.svg
    [ width <| toString model.width
    , height <| toString model.height
    ]
    (background model.width model.height :: (List.map pieceView model.pieces))


pieceView : ( Name, Piece.Model ) -> Svg.Svg Msg
pieceView ( name, piece ) =
  Piece.view piece |> Html.map (PieceMsg name)


background : Int -> Int -> Svg.Svg Msg
background width height =
  Svg.rect
    [ Svg.width <| toString width
    , Svg.height <| toString height
    , fill "#EEEEEE"
    ]
    []


debugInfo : Model -> Html.Html Msg
debugInfo model =
  Html.div
    []
    [ Html.text <| "height = " ++ toString model.height ++ " width = " ++ toString model.width
    , Html.ul
        []
        (List.map (\item -> Html.li [] [ (Html.text << toString) item ]) model.pieces)
    ]
