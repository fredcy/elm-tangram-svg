module Model exposing (..)

import Task
import Window


--

import Colors
import Types exposing (..)
import Piece.Model as Piece
import Piece.Types as Piece


type alias Model =
    { pieces : List ( Name, Piece.Model )
    , size : Window.Size
    , shift : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        pieces =
            [ ( "bigTri1", (Piece.init (Piece.Triangle (Colors.elmTurquoise) 100.0) (Piece.Position 200 300) 0) )
            , ( "bigTri2", (Piece.init (Piece.Triangle (Colors.elmGray) 100.0) (Piece.Position 150 250) 90) )
            , ( "medTri", (Piece.init (Piece.Triangle (Colors.elmTurquoise) (100.0 / sqrt 2)) (Piece.Position 275 175) 45) )
            , ( "smTri1", (Piece.init (Piece.Triangle (Colors.elmOrange) (100.0 / 2)) (Piece.Position 275 300) -90) )
            , ( "smTri2", (Piece.init (Piece.Triangle (Colors.elmOrange) (100.0 / 2)) (Piece.Position 200 225) 180) )
            , ( "square", (Piece.init (Piece.Square (Colors.elmGreen) 100.0) (Piece.Position 250 250) 180) )
            , ( "para", (Piece.init (Piece.Parallelogram (Colors.elmGreen) 100.0) (Piece.Position 175 175) 180) )
            ]
    in
        ( { pieces = pieces
          , size = Window.Size 600 600
          , shift = False
          }
        , Task.perform (always Error) WindowSize Window.size
        )
