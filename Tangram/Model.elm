module Tangram.Model exposing (Model, init, subscriptions, tangramPieces, storageName)

import Task
import Window
import Keyboard
import LocalStorage


--

import Colors
import Tangram.Types exposing (..)
import Piece.Model as Piece
import Piece.Types as Piece


type alias Model =
    { pieces : List ( Name, Piece.Model )
    , size : Window.Size
    , shift : Bool
    , showingLayout : Bool
    , name : String
    }


storageName : String -> String
storageName name =
    "tangram-" ++ name


defaultName : String
defaultName =
    "default"


init : ( Model, Cmd Msg )
init =
    let
        pieces =
            tangramPieces
    in
        ( { pieces = pieces
          , size = Window.Size 600 600
          , shift = False
          , showingLayout = False
          , name = defaultName
          }
        , Cmd.batch
            [ Task.perform (always Error) WindowSize Window.size
            , Task.perform (always Error) GetLayout (LocalStorage.get (storageName defaultName))
            ]
        )


tangramPieces : List ( String, Piece.Model )
tangramPieces =
    [ ( "bigTri1", (Piece.init (Piece.Triangle (Colors.elmTurquoise) 100.0) (Piece.Position 200 300) 0) )
    , ( "bigTri2", (Piece.init (Piece.Triangle (Colors.elmGray) 100.0) (Piece.Position 150 250) 0.25) )
    , ( "medTri", (Piece.init (Piece.Triangle (Colors.elmTurquoise) (100.0 / sqrt 2)) (Piece.Position 275 175) 0.125) )
    , ( "smTri1", (Piece.init (Piece.Triangle (Colors.elmOrange) (100.0 / 2)) (Piece.Position 275 300) -0.25) )
    , ( "smTri2", (Piece.init (Piece.Triangle (Colors.elmOrange) (100.0 / 2)) (Piece.Position 200 225) 0.5) )
    , ( "square", (Piece.init (Piece.Square (Colors.elmGreen) 100.0) (Piece.Position 250 250) 0.5) )
    , ( "para", (Piece.init (Piece.Parallelogram (Colors.elmGreen) 100.0) (Piece.Position 175 175) 0.5) )
    ]


{-| Gather the subscriptions of each of the components into a single Batch.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mapSubs ( name, piece ) =
            Piece.subscriptions piece |> Sub.map (PieceMsg name)

        reSize =
            Window.resizes WindowSize

        keyDowns =
            Keyboard.downs KeyDown

        keyUps =
            Keyboard.ups KeyUp
    in
        [ keyUps, keyDowns, reSize ] ++ List.map mapSubs model.pieces |> Sub.batch
