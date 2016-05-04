module Main exposing (..)

import Html
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import Task
import VirtualDom
import Window


--import Svg.Lazy

import Colors
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
  , size : Window.Size
  }


init : ( Model, Cmd Msg )
init =
  let
    pieces =
      [ ( "bigTri1", (Piece.init (Piece.Triangle (Colors.toCss Colors.elmTurquoise) 100.0) (Piece.Position 200 300) 0) )
      , ( "bigTri2", (Piece.init (Piece.Triangle (Colors.toCss Colors.elmGray) 100.0) (Piece.Position 150 250) 90) )
      ]
  in
    ( { pieces = pieces, size = Window.Size 600 600 }
    , Task.perform (always Error) WindowSize Window.size
    )


type Msg
  = PieceMsg Name Piece.Msg
  | WindowSize Window.Size
  | Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg |> Debug.log "msg" of
    PieceMsg name pieceMsg ->
      let
        ( pieces', cmds ) =
          updatePieces name pieceMsg model.pieces
      in
        ( { model | pieces = pieces' }, Cmd.batch cmds )

    WindowSize wsize ->
      let
        -- Allow for horizontal margin (?) and give vertical room for additional content
        width =
          wsize.width - 16

        height =
          wsize.height - 300
      in
        ( { model | size = Window.Size width height }, Cmd.none )

    Error ->
      ( model, Cmd.none )


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

    reSize =
      Window.resizes WindowSize
  in
    reSize :: List.map mapSubs model.pieces |> Sub.batch



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
    [ width <| toString model.size.width
    , height <| toString model.size.height
    ]
    (background model.size.width model.size.height :: (List.map pieceView model.pieces))


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
    [ Html.text <| "size = " ++ toString model.size
    , Html.ul
        []
        (List.map (\item -> Html.li [] [ (Html.text << toString) item ]) model.pieces)
    ]
